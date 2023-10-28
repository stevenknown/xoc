#Copyright 2011 Alibaba Group
LOCAL_PATH := $(call my-dir)

RELATIVE_VMKID := $(LOCAL_PATH)/..

MY_INCLUDE_FILES := \
	external/zlib \
	system/core/include \
	$(RELATIVE_VMKID)/clibs/basic/include \
	$(RELATIVE_VMKID)/clibs/basic/arch/common

MY_SRC_FILES := \
	basic/arch/android/dl/*.c \
	basic/arch/android/io/*.c \
	basic/arch/android/math/*.c \
	basic/arch/android/mem/*.c \
	basic/arch/android/sys/*.c \
	basic/arch/android/thread/*.c \
	basic/arch/android/time/*.c \
	basic/arch/android/trace/*.c \
	basic/arch/android/utils/*.c \
	basic/arch/android/zip/*.c \
	basic/arch/common/zip/*.c \
	basic/arch/common/str/*.c \
	basic/arch/common/io/*.c \
	basic/arch/common/errno/*.c \
	basic/arch/common/fdlibm/*.c \
	basic/arch/common/math/*.c \
	basic/arch/common/mem/*.c \
	basic/arch/common/sys/*.c \
	basic/arch/common/thread/*.c \
	basic/arch/common/time/*.c \
	basic/arch/common/trace/*.c \
	basic/arch/common/utf/*.c \
	basic/arch/common/utils/*.c \

MY_LOCAL_CFLAGS := \
	-DLOG_TAG=\"CLIBS\" \
	-D_GNU_SOURCE=1

########################################################
#
# target version clibs share lib for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
include $(RELATIVE_VMKID)/config.mk

LOCAL_CFLAGS += $(MY_LOCAL_CFLAGS)

LOCAL_C_INCLUDES := $(MY_INCLUDE_FILES)

LOCAL_SRC_FILES := $(foreach F, $(MY_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_PRELINK_MODULE 	:= false
LOCAL_SHARED_LIBRARIES  := \
	libz \
	libdl \
	liblog \
	libcutils \
	libutils \
	libbinder

ifeq ($(TVM_RELEASE), true)
LOCAL_CFLAGS += -DLOG_NDEBUG=1
endif
 
## Help to debug vmkid.
## This MACRO defined in vmkid/config.mk: TARGET_lemur_CFLAGS
## you can remove '-Os' for debug
LOCAL_ARM_MODE := lemur
LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := libaoc_clibs

include $(BUILD_SHARED_LIBRARY)

########################################################
#
# host version clibs share lib for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
include $(RELATIVE_VMKID)/config.mk

LOCAL_CFLAGS += $(MY_LOCAL_CFLAGS)
LOCAL_CFLAGS += -DHOST_LEMUR

LOCAL_C_INCLUDES += $(MY_INCLUDE_FILES)
LOCAL_SRC_FILES := $(foreach F, $(MY_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := libaoc_clibs

LOCAL_LDLIBS += -lz -lpthread -ldl -lm -lrt

LOCAL_MULTILIB := both 
include $(BUILD_HOST_SHARED_LIBRARY)

########################################################
#
# host version clibs share lib for LEMUR_PROJECT
#
########################################################
include $(CLEAR_VARS)
#must include vmkid/config.mk after CLEAR_VARS
include $(RELATIVE_VMKID)/config.mk

LOCAL_CFLAGS += $(MY_LOCAL_CFLAGS)
LOCAL_CFLAGS += -DHOST_LEMUR

LOCAL_C_INCLUDES += $(MY_INCLUDE_FILES)
LOCAL_SRC_FILES := $(foreach F, $(MY_SRC_FILES), $(addprefix $(dir $(F)),$(notdir $(wildcard $(LOCAL_PATH)/$(F)))))

LOCAL_MODULE_TAGS := optional
LOCAL_MODULE := libaoc_clibs

LOCAL_LDLIBS += -lz -lpthread -ldl -lm -lrt

include $(BUILD_HOST_STATIC_LIBRARY)

