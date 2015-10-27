#The following code is written by Alibaba Group:

#some global macros

ifeq ($(PLATFORM_SDK_VERSION), 8)
ANDROID_MAJOR_VERSION_NUM := 22 
endif
ifeq ($(PLATFORM_SDK_VERSION), 9)
ANDROID_MAJOR_VERSION_NUM := 22 
endif
ifeq ($(PLATFORM_SDK_VERSION), 10)
ANDROID_MAJOR_VERSION_NUM := 23
endif
ifeq ($(PLATFORM_SDK_VERSION), 14)
ANDROID_MAJOR_VERSION_NUM := 40
endif
ifeq ($(PLATFORM_SDK_VERSION), 15)
ANDROID_MAJOR_VERSION_NUM := 40
endif
ifeq ($(PLATFORM_SDK_VERSION), 16)
ANDROID_MAJOR_VERSION_NUM := 41
endif
ifeq ($(PLATFORM_SDK_VERSION), 17)
ANDROID_MAJOR_VERSION_NUM := 42
endif
ifeq ($(PLATFORM_SDK_VERSION), 18)
ANDROID_MAJOR_VERSION_NUM := 43
endif
ifeq ($(PLATFORM_SDK_VERSION), 19)
ANDROID_MAJOR_VERSION_NUM := 44
endif
ifeq ($(PLATFORM_SDK_VERSION), 21)
ANDROID_MAJOR_VERSION_NUM := 50
endif

ifeq ($(ANDROID_MAJOR_VERSION_NUM),)
    COMPARE_RESULT := $(shell echo ${PLATFORM_SDK_VERSION} \>= 15 | bc)
    ifeq ($(COMPARE_RESULT), 1)
    ANDROID_MAJOR_VERSION_NUM := 40
    endif
endif

ifeq ($(ANDROID_MAJOR_VERSION_NUM),)
$(error ANDROID_MAJOR_VERSION_NUM does not get a value for sdk version $(PLATFORM_SDK_VERSION))
endif

TARGET_lemur_CFLAGS :=
TARGET_lemur_CFLAGS += -mpoke-function-name 
TARGET_lemur_CFLAGS += -fomit-frame-pointer 
TARGET_lemur_CFLAGS += -fno-strict-aliasing    
TARGET_lemur_CFLAGS += -funswitch-loops     
TARGET_lemur_CFLAGS += -finline-limit=1200
TARGET_lemur_CFLAGS += -finline-functions

LOCAL_CFLAGS := \
	-DANDROID_MAJOR_VERSION=$(ANDROID_MAJOR_VERSION_NUM) \
	-DARCH=\"$(TARGET_ARCH)\"

## release variant has a higher priviledge than building variant
## TVM_RELEASE seemed as debug's trigger, later may be more complicated
building_variant := $(filter user userdebug,$(TARGET_BUILD_VARIANT))
ifneq ($(building_variant),)
TVM_RELEASE := true
else
TVM_RELEASE := false
endif

release_variant := $(filter on off,$(VM_DEBUG))
ifeq (off,$(release_variant))
TVM_RELEASE := true
$(info "setting lemur in release mode with VM_DEBUG");
else
ifeq (on,$(release_variant))
TVM_RELEASE := false
$(info "setting lemur in debug mode with VM_DEBUG");
endif
endif
