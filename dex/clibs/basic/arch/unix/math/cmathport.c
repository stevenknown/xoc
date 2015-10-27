/*Copyright 2011 Alibaba Group*/
/*
 *  cmathport.c
 *  iplug.iphone
 *
 *  Created by Misa.Z on 3/31/10.
 *  Copyright 2010 VMKid. All rights reserved.
 *
 */

#include <stdlib.h>
#include "std/cstd.h"

Int32 cRandPort() {
    return rand();
}

void cSRandPort(UInt32 seed) {
    srand(seed);
}
