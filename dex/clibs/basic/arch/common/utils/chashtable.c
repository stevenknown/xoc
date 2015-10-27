/*Copyright 2011 Alibaba Group*/
#include "utils/chashtable.h"
#include "mem/cmem.h"

typedef struct {
    /** 0 ~ 30 bits: next node index
     *  31 bit: idle flag -- 0: used    1: idle
     */
    UInt32 next;
    UInt32 hash;
    UInt32 keySize;
    void *key;
    UInt32 valueSize;
    /** idle: number of consequent idle node
     *  used: node data pointer
     */
    void *value;
}HashTableNode;

typedef struct {
    UInt32 hashWidth;
    UInt32* nodeArray;
    UInt32 numItems;

    HashCodeFunc genHashCode;
    HashMallocFunc alloc;
    HashFreeFunc free;
    UInt32 mode;
    UInt32 maxItems;
    Float32 loadFactor;

    UInt32 numAllNodes;
    HashTableNode* allNodes;
    //index in nodes pool, first elements of idle nodes list
    UInt32 idleNodes;
}HashTable;

#define INVALID_NODE_INDEX 0xffffffff
#define INVALID_USED_NODE_INDEX 0x7fffffff
#define MASK_IDLE_NODE_FLAG 0x7fffffff
#define IDLE_NODE_FLAG 0x80000000

#define INIT_HASH_WIDTH 32

#define HASH_ALLOC(sz) ht->alloc(sz)
#define HASH_FREE(p) ht->free(p)


static bool initNodesPool(HashTable* ht, ULong size) {
    HashTableNode* tmp;

    ht->allNodes = (HashTableNode*)HASH_ALLOC(size* sizeof(HashTableNode));
    if(ht->allNodes == NULL)
        return false;
    ht->numAllNodes = size;
    ht->idleNodes = 0;
    tmp = ht->allNodes;
    tmp->next = INVALID_NODE_INDEX;
    tmp->value = (void*)size;
    return true;
}

static bool expandNodesPool(HashTable* ht) {
    UInt32 newNum;
    HashTableNode* newPool;
    HashTableNode* tmp;

    newNum = ht->numAllNodes << 1;
    newPool = (HashTableNode*)HASH_ALLOC(newNum * sizeof(HashTableNode));
    if(newPool == NULL)
        return false;
    cMemcpy(newPool, ht->allNodes, ht->numAllNodes * sizeof(HashTableNode));
    tmp = newPool + ht->numAllNodes;
    tmp->next = INVALID_NODE_INDEX;
    tmp->value = (void*)(ULong)ht->numAllNodes;
    HASH_FREE(ht->allNodes);
    ht->allNodes = newPool;
    ht->idleNodes = ht->numAllNodes;
    ht->numAllNodes = newNum;
    return true;
}

static HashTableNode* getIdleNode(HashTable* ht) {
    HashTableNode* node;

    if(ht->idleNodes == INVALID_USED_NODE_INDEX) {
        if(!expandNodesPool(ht))
            return false;
    }

    node = ht->allNodes + ht->idleNodes;
    if((UInt32)(ULong)node->value > 1) {
        node[1].next = node->next;
        node[1].value = (void*)(ULong)(((UInt32)(ULong)node->value) - 1);
        ++ht->idleNodes;
    }
    else
        ht->idleNodes = (UInt32)(node->next & MASK_IDLE_NODE_FLAG);
    node->next = INVALID_USED_NODE_INDEX;
    node->value = NULL;
    node->hash = 0;
    return node;
}

static void releaseNode(HashTable* ht, HashTableNode* node) {
    node->next = ht->idleNodes;
    node->value = (void*)1;
    ht->idleNodes = node - ht->allNodes;
}

static void freeNodesPool(HashTable* ht) {
    HASH_FREE(ht->allNodes);
}

static UInt32 defaultHashFunc(const void* data, UInt32 len) {
    const BYTE* p;
    UInt32 hash = 5381;

    p = (const BYTE*)data;
    while(len) {
        hash = (hash + (hash << 5)) ^ *p;
        ++p;
        --len;
    }
    return hash;
}

static HashTableNode* searchHashTableNode(HashTable* ht, const void* key, UInt32 keySize) {
    UInt32 hash;
    UInt32 node;
    HashTableNode* htnode;

    hash = ht->genHashCode(key, keySize);
    node = ht->nodeArray[hash % ht->hashWidth];
    while(node != INVALID_USED_NODE_INDEX) {
        htnode = ht->allNodes + node;
        if(htnode->keySize == keySize) {
            if(htnode->key == key)
                return htnode;
            if(keySize && cMemcmp(htnode->key, key, keySize) == 0)
                return htnode;
        }
        node = htnode->next;
    }
    return NULL;
}

//debug
/**
static void printHashInfo(HashTableHandle hth) {
    HashTable* ht;
    UInt32 i;
    UInt32 node;
    HashTableNode* htnode;

    ht = (HashTable*)hth;
    for(i=0; i<ht->hashWidth; ++i) {
        node = ht->nodeArray[i];
        if(node != INVALID_USED_NODE_INDEX)
            printf("%u: ", i);
        while(node != INVALID_USED_NODE_INDEX) {
            printf("%u ", node);
            htnode = ht->allNodes + node;
            node = htnode->next;
            if(node != INVALID_USED_NODE_INDEX)
                printf("--> ");
            else
                printf("\n");
        }
    }
}
*/
//debug

static void rehash(HashTable* ht) {
    UInt32 newWidth;
    UInt32* newNodes;
    UInt32 i;
    UInt32 node;
    UInt32 tmp;
    HashTableNode* htnode;
    UInt32 newIndex;

    newWidth = (ht->hashWidth << 3) / 5 + 13;
    newNodes = (UInt32*)HASH_ALLOC(newWidth << 2);
    if(newNodes == NULL)
        return;
    for(i=0; i<newWidth; ++i)
        newNodes[i] = INVALID_USED_NODE_INDEX;
    for(i=0; i<ht->hashWidth; ++i) {
        node = ht->nodeArray[i];
        while(node != INVALID_USED_NODE_INDEX) {
            tmp = node;
            htnode = ht->allNodes + tmp;
            node = htnode->next;
            newIndex = htnode->hash % newWidth;
            htnode->next = newNodes[newIndex];
            newNodes[newIndex] = tmp;
        }
    }
    HASH_FREE(ht->nodeArray);
    ht->nodeArray = newNodes;
    ht->hashWidth = newWidth;
    ht->maxItems = (UInt32)(newWidth * ht->loadFactor);
}

HashTableHandle cHashCreateEx(HashCodeFunc hashFunc, HashMallocFunc mallocFunc, HashFreeFunc freeFunc,
        UInt32 initCapacity, Float32 loadFactor, UInt32 mode) {
    HashTable* ht;
    UInt32 i;
    HashMallocFunc pfnAlloc;

    if(mallocFunc == NULL)
        pfnAlloc = cMallocRaw;
    else
        pfnAlloc = mallocFunc;

    ht = (HashTable*)pfnAlloc(sizeof(HashTable));
    if(ht == NULL)
        return 0;
    ht->alloc = pfnAlloc;
    if(freeFunc == NULL)
        ht->free = cFree;
    else
        ht->free = freeFunc;

    if(initCapacity == 0)
        ht->hashWidth = INIT_HASH_WIDTH;
    else
        ht->hashWidth = initCapacity;
    ht->nodeArray = (UInt32*)HASH_ALLOC(ht->hashWidth << 2);
    if(ht->nodeArray == NULL) {
        HASH_FREE(ht);
        return 0;
    }
    for(i=0; i<ht->hashWidth; ++i)
        ht->nodeArray[i] = INVALID_USED_NODE_INDEX;
    if(hashFunc)
        ht->genHashCode = hashFunc;
    else
        ht->genHashCode = defaultHashFunc;
    if(!initNodesPool(ht, ht->hashWidth)) {
        HASH_FREE(ht->nodeArray);
        HASH_FREE(ht);
        return 0;
    }
    ht->numItems = 0;
    if(loadFactor == 0.0)
        loadFactor = 0.75;
    ht->maxItems = (UInt32)(ht->hashWidth * loadFactor);
    ht->loadFactor = loadFactor;
    ht->mode = mode;
    return (HashTableHandle)ht;
}

HashTableHandle cHashCreate(HashCodeFunc hashFunc, UInt32 initCapacity, Float32 loadFactor, UInt32 mode) {
    return cHashCreateEx(hashFunc, NULL, NULL, initCapacity, loadFactor, mode);
}

bool cHashInsert(HashTableHandle hth, const void* key, UInt32 keySize, const void* data, UInt32 dataSize) {
    HashTable* ht;
    UInt32 index;
    HashTableNode* newNode;

    if(hth == 0 || key == NULL || data == NULL)
        return false;
    ht = (HashTable*)hth;
    newNode = getIdleNode(ht);
    if(newNode == NULL)
        return false;
    if(ht->mode == CHASH_MODE_STORE_NONE) {
        newNode->keySize = keySize;
        newNode->valueSize = dataSize;
        newNode->key = (void*)key;
        newNode->value = (void*)data;
    }
    else if(ht->mode == CHASH_MODE_STORE_KEY) {
        BYTE* nodeData;

        nodeData = (BYTE*)HASH_ALLOC(keySize);
        if(nodeData == NULL) {
            releaseNode(ht, newNode);
            return false;
        }
        cMemcpy(nodeData, key, keySize);
        newNode->keySize = keySize;
        newNode->valueSize = dataSize;
        newNode->key = nodeData;
        newNode->value = (void*)data;
    }
    else {
        BYTE* nodeData;

        nodeData = (BYTE*)HASH_ALLOC(keySize + dataSize);
        if(nodeData == NULL) {
            releaseNode(ht, newNode);
            return false;
        }
        cMemcpy(nodeData, key, keySize);
        cMemcpy(nodeData + keySize, data, dataSize);
        newNode->keySize = keySize;
        newNode->valueSize = dataSize;
        newNode->key = nodeData;
        newNode->value = nodeData + keySize;
    }

    newNode->hash = ht->genHashCode(key, keySize);
    index = newNode->hash % ht->hashWidth;
    newNode->next = ht->nodeArray[index];
    ht->nodeArray[index] = newNode - ht->allNodes;
    ++ht->numItems;
    if(ht->numItems >= ht->maxItems)
        rehash(ht);
    return true;
}

void* cHashGet(HashTableHandle hth, const void* key, UInt32 keySize, UInt32* dataSize) {
    HashTable* ht;
    HashTableNode* htnode;

    if(hth == 0 || key == NULL)
        return NULL;
    ht = (HashTable*)hth;
    htnode = searchHashTableNode(ht, key, keySize);
    if(htnode) {
        if(dataSize)
            *dataSize = htnode->valueSize;
        return htnode->value;
    }
    return NULL;
}

void cHashRemove(HashTableHandle hth, const void* key, UInt32 keySize) {
    HashTable* ht;
    UInt32 hash;
    UInt32 node;
    UInt32 prevNode;
    HashTableNode* htnode;

    if(hth == 0 || key == NULL)
        return;
    ht = (HashTable*)hth;
    hash = ht->genHashCode(key, keySize);
    node = ht->nodeArray[hash % ht->hashWidth];
    prevNode = INVALID_USED_NODE_INDEX;
    while(node != INVALID_USED_NODE_INDEX) {
        htnode = ht->allNodes + node;
        if(htnode->keySize == keySize) {
            if(htnode->key == key)
                break;
            if(keySize && cMemcmp(htnode->key, key, keySize) == 0)
                break;
        }
        prevNode = node;
        node = htnode->next;
    }
    if(node != INVALID_USED_NODE_INDEX) {
        if(prevNode != INVALID_USED_NODE_INDEX) {
            HashTableNode* prevhtnode;
            prevhtnode = ht->allNodes + prevNode;
            prevhtnode->next = htnode->next;
        }
        else
            ht->nodeArray[hash % ht->hashWidth] = htnode->next;
        if(ht->mode != CHASH_MODE_STORE_NONE)
            HASH_FREE(htnode->key);
        releaseNode(ht, htnode);
        --ht->numItems;
    }
}

void cHashTraversal(HashTableHandle hth, TraversalActionFunc action, void* arg) {
    HashTable* ht;
    UInt32 i;
    UInt32 node;
    HashTableNode* htnode;

    if(hth == 0 || action == NULL)
        return;
    ht = (HashTable*)hth;
    for(i=0; i<ht->hashWidth; ++i) {
        node = ht->nodeArray[i];
        while(node != INVALID_USED_NODE_INDEX) {
            htnode = ht->allNodes + node;
            node = htnode->next;
            if(!action(htnode->key, htnode->keySize, htnode->value, htnode->valueSize, arg))
                return;
        }
    }
}

void cHashFree(HashTableHandle hth) {
    HashTable* ht;

    if(hth == 0)
        return;
    ht = (HashTable*)hth;
    if(ht->mode != CHASH_MODE_STORE_NONE) {
        UInt32 i;
        UInt32 node;
        HashTableNode* htnode;

        for(i=0; i<ht->hashWidth; ++i) {
            node = ht->nodeArray[i];
            while(node != INVALID_USED_NODE_INDEX) {
                htnode = ht->allNodes + node;
                node = htnode->next;
                HASH_FREE(htnode->key);
            }
        }
    }
    HASH_FREE(ht->nodeArray);
    freeNodesPool(ht);
    HASH_FREE(ht);
}

UInt32 cHashCount(HashTableHandle hth) {
    HashTable* ht;

    if(hth == 0)
        return 0;
    ht = (HashTable*)hth;
    return ht->numItems;
}
