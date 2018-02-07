
LOCAL_PATH := $(call my-dir)
RELATIVE_AOC := $(LOCAL_PATH)/../..

X_INCLUDE_FILES := \
    dalvik \
    ${LOCAL_PATH} \
    ${LOCAL_PATH}/clibs/basic/include \
    ${LOCAL_PATH}/clibs/basic/arch/common \
    ${LOCAL_PATH}/dex2dex \
    ${LOCAL_PATH}/include \
    ${LOCAL_PATH}/lir \
    ${LOCAL_PATH}/linealloc \
    ${LOCAL_PATH}/../com \
    ${LOCAL_PATH}/../reader \
    ${LOCAL_PATH}/../opt


#X_LOCAL_CFLAGS := \
	-DLOG_TAG=\"dexpro\" \
	-D_GNU_SOURCE=1\
	-D_ENABLE_LOG_\
	-D_VMWARE_DEBUG_\
	-O2 -DFOR_DEX\
	-Wno-empty-body\
	-Wreorder -Wno-write-strings -Wsign-promo -Werror=pointer-to-int-cast -Wparentheses\
	-Wformat -Wsign-compare -Wpointer-arith -Wno-multichar -Winit-self\
	-Wuninitialized -Wmaybe-uninitialized -Wtype-limits\
	-Wstrict-overflow -Wstrict-aliasing=3 -finline-limit=10000000 -Wswitch

#X_LOCAL_CFLAGS := \
	-DLOG_TAG=\"dexpro\" \
	-D_GNU_SOURCE=1\
	-D_VMWARE_DEBUG_\
	-D_ENABLE_LOG_\
	-ggdb -O0 -D_DEBUG_ -DFOR_DEX\
	-Wno-empty-body\
	-Wreorder -Wno-write-strings -Wsign-promo -Werror=pointer-to-int-cast -Wparentheses\
	-Wformat -Wsign-compare -Wpointer-arith -Wno-multichar -Winit-self\
	-Wuninitialized -Wmaybe-uninitialized -Wtype-limits\
	-Wstrict-overflow -Wstrict-aliasing=3 -finline-limit=10000000 -Wswitch \
	-Werror=overloaded-virtual

	#-DANDROID_SMP=1 -std=gnu++11 -DAOC_COMPAT -fpermissive

########################################################
#
# host version dex2dex share lib for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
#include $(RELATIVE_VMKID)/config.mk
include $(RELATIVE_AOC)/xoc/dex/config.mk

X_SRC_FILES:= \
    *.cpp\
    dex2dex/*.cpp\
    linealloc/*.cpp\
    lir/*.cpp\
    ../opt/*.cpp\
    ../reader/*.cpp\
    ../com/*.cpp

LOCAL_CFLAGS += $(X_LOCAL_CFLAGS)

LOCAL_C_INCLUDES := $(X_INCLUDE_FILES)
LOCAL_SRC_FILES := $(foreach F, $(X_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := libaoc_xoc

LOCAL_STATIC_LIBRARIES := libdex libz libcutils liblog

LOCAL_SHARED_LIBRARIES := libaoc_clibs  ##libaoc-compiler

LOCAL_MULTILIB := both
include $(BUILD_HOST_SHARED_LIBRARY)

########################################################
#
# target version dex2dex static lib for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
include $(RELATIVE_AOC)/xoc/dex/config.mk

X_SRC_FILES:= \
    dex2dex/*.cpp\
    linealloc/*.cpp\
    lir/*.cpp\
    ../opt/*.cpp\
    ../reader/*.cpp\
    ../com/*.cpp\
    *.cpp

LOCAL_CFLAGS += $(X_LOCAL_CFLAGS)

LOCAL_C_INCLUDES := $(X_INCLUDE_FILES)

LOCAL_SRC_FILES := $(foreach F, $(X_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := libaoc_xoc

LOCAL_STATIC_LIBRARIES := libdex libz libcutils liblog

LOCAL_SHARED_LIBRARIES := libaoc_clibs ##libaoc-compiler

include $(BUILD_SHARED_LIBRARY)

########################################################
#
# host version dex2dex executable for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
include $(RELATIVE_AOC)/xoc/dex/config.mk

X_SRC_FILES:= dex2dex/main.cpp

LOCAL_C_INCLUDES := $(X_INCLUDE_FILES)
LOCAL_SRC_FILES := $(foreach F, $(X_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_CFLAGS += $(X_LOCAL_CFLAGS)
LOCAL_CFLAGS += -DHOST_LEMUR

LOCAL_STATIC_LIBRARIES := libdex libz liblog libcutils

LOCAL_SHARED_LIBRARIES := libaoc_clibs libaoc_xoc

LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := dexpro

include $(BUILD_HOST_EXECUTABLE)
