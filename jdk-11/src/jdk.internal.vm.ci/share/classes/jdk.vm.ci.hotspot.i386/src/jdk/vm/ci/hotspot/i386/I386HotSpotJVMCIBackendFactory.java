/*
 * Copyright (c) 2012, 2016, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package jdk.vm.ci.hotspot.i386;

import static jdk.vm.ci.common.InitTimer.timer;

import java.util.EnumSet;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.code.Architecture;
import jdk.vm.ci.code.RegisterConfig;
import jdk.vm.ci.code.TargetDescription;
import jdk.vm.ci.code.stack.StackIntrospection;
import jdk.vm.ci.common.InitTimer;
import jdk.vm.ci.hotspot.HotSpotCodeCacheProvider;
import jdk.vm.ci.hotspot.HotSpotConstantReflectionProvider;
import jdk.vm.ci.hotspot.HotSpotJVMCIBackendFactory;
import jdk.vm.ci.hotspot.HotSpotJVMCIRuntimeProvider;
import jdk.vm.ci.hotspot.HotSpotMetaAccessProvider;
import jdk.vm.ci.hotspot.HotSpotStackIntrospection;
import jdk.vm.ci.meta.ConstantReflectionProvider;
import jdk.vm.ci.runtime.JVMCIBackend;

public class I386HotSpotJVMCIBackendFactory implements HotSpotJVMCIBackendFactory {

    protected EnumSet<I386.CPUFeature> computeFeatures(I386HotSpotVMConfig config) {
        // Configure the feature set using the HotSpot flag settings.
        EnumSet<I386.CPUFeature> features = EnumSet.noneOf(I386.CPUFeature.class);
        if ((config.vmVersionFeatures & config.i3863DNOWPREFETCH) != 0) {
            features.add(I386.CPUFeature.AMD_3DNOW_PREFETCH);
        }
        assert config.useSSE >= 2 : "minimum config for x64";
        features.add(I386.CPUFeature.SSE);
        features.add(I386.CPUFeature.SSE2);
        if ((config.vmVersionFeatures & config.i386SSE3) != 0) {
            features.add(I386.CPUFeature.SSE3);
        }
        if ((config.vmVersionFeatures & config.i386SSSE3) != 0) {
            features.add(I386.CPUFeature.SSSE3);
        }
        if ((config.vmVersionFeatures & config.i386SSE4A) != 0) {
            features.add(I386.CPUFeature.SSE4A);
        }
        if ((config.vmVersionFeatures & config.i386SSE41) != 0) {
            features.add(I386.CPUFeature.SSE4_1);
        }
        if ((config.vmVersionFeatures & config.i386SSE42) != 0) {
            features.add(I386.CPUFeature.SSE4_2);
        }
        if ((config.vmVersionFeatures & config.i386POPCNT) != 0) {
            features.add(I386.CPUFeature.POPCNT);
        }
        if ((config.vmVersionFeatures & config.i386LZCNT) != 0) {
            features.add(I386.CPUFeature.LZCNT);
        }
        if ((config.vmVersionFeatures & config.i386ERMS) != 0) {
            features.add(I386.CPUFeature.ERMS);
        }
        if ((config.vmVersionFeatures & config.i386AVX) != 0) {
            features.add(I386.CPUFeature.AVX);
        }
        if ((config.vmVersionFeatures & config.i386AVX2) != 0) {
            features.add(I386.CPUFeature.AVX2);
        }
        if ((config.vmVersionFeatures & config.i386AES) != 0) {
            features.add(I386.CPUFeature.AES);
        }
        if ((config.vmVersionFeatures & config.i3863DNOWPREFETCH) != 0) {
            features.add(I386.CPUFeature.AMD_3DNOW_PREFETCH);
        }
        if ((config.vmVersionFeatures & config.i386BMI1) != 0) {
            features.add(I386.CPUFeature.BMI1);
        }
        if ((config.vmVersionFeatures & config.i386BMI2) != 0) {
            features.add(I386.CPUFeature.BMI2);
        }
        if ((config.vmVersionFeatures & config.i386RTM) != 0) {
            features.add(I386.CPUFeature.RTM);
        }
        if ((config.vmVersionFeatures & config.i386ADX) != 0) {
            features.add(I386.CPUFeature.ADX);
        }
        if ((config.vmVersionFeatures & config.i386AVX512F) != 0) {
            features.add(I386.CPUFeature.AVX512F);
        }
        if ((config.vmVersionFeatures & config.i386AVX512DQ) != 0) {
            features.add(I386.CPUFeature.AVX512DQ);
        }
        if ((config.vmVersionFeatures & config.i386AVX512PF) != 0) {
            features.add(I386.CPUFeature.AVX512PF);
        }
        if ((config.vmVersionFeatures & config.i386AVX512ER) != 0) {
            features.add(I386.CPUFeature.AVX512ER);
        }
        if ((config.vmVersionFeatures & config.i386AVX512CD) != 0) {
            features.add(I386.CPUFeature.AVX512CD);
        }
        if ((config.vmVersionFeatures & config.i386AVX512BW) != 0) {
            features.add(I386.CPUFeature.AVX512BW);
        }
        if ((config.vmVersionFeatures & config.i386AVX512VL) != 0) {
            features.add(I386.CPUFeature.AVX512VL);
        }
        if ((config.vmVersionFeatures & config.i386SHA) != 0) {
            features.add(I386.CPUFeature.SHA);
        }
        if ((config.vmVersionFeatures & config.i386FMA) != 0) {
            features.add(I386.CPUFeature.FMA);
        }
        return features;
    }

    protected EnumSet<I386.Flag> computeFlags(I386HotSpotVMConfig config) {
        EnumSet<I386.Flag> flags = EnumSet.noneOf(I386.Flag.class);
        if (config.useCountLeadingZerosInstruction) {
            flags.add(I386.Flag.UseCountLeadingZerosInstruction);
        }
        if (config.useCountTrailingZerosInstruction) {
            flags.add(I386.Flag.UseCountTrailingZerosInstruction);
        }
        return flags;
    }

    protected TargetDescription createTarget(I386HotSpotVMConfig config) {
        final int stackFrameAlignment = 16;
        final int implicitNullCheckLimit = 4096;
        final boolean inlineObjects = true;
        Architecture arch = new I386(computeFeatures(config), computeFlags(config));
        return new TargetDescription(arch, true, stackFrameAlignment, implicitNullCheckLimit, inlineObjects);
    }

    protected HotSpotConstantReflectionProvider createConstantReflection(HotSpotJVMCIRuntimeProvider runtime) {
        return new HotSpotConstantReflectionProvider(runtime);
    }

    protected RegisterConfig createRegisterConfig(I386HotSpotVMConfig config, TargetDescription target) {
        return new I386HotSpotRegisterConfig(target, config.useCompressedOops, config.windowsOs);
    }

    protected HotSpotCodeCacheProvider createCodeCache(HotSpotJVMCIRuntimeProvider runtime, TargetDescription target, RegisterConfig regConfig) {
        return new HotSpotCodeCacheProvider(runtime, runtime.getConfig(), target, regConfig);
    }

    protected HotSpotMetaAccessProvider createMetaAccess(HotSpotJVMCIRuntimeProvider runtime) {
        return new HotSpotMetaAccessProvider(runtime);
    }

    @Override
    public String getArchitecture() {
        return "I386";
    }

    @Override
    public String toString() {
        return "JVMCIBackend:" + getArchitecture();
    }

    @SuppressWarnings("try")
    public JVMCIBackend createJVMCIBackend(HotSpotJVMCIRuntimeProvider runtime, JVMCIBackend host) {
        assert host == null;
        I386HotSpotVMConfig config = new I386HotSpotVMConfig(runtime.getConfigStore());
        TargetDescription target = createTarget(config);

        RegisterConfig regConfig;
        HotSpotCodeCacheProvider codeCache;
        ConstantReflectionProvider constantReflection;
        HotSpotMetaAccessProvider metaAccess;
        StackIntrospection stackIntrospection;
        try (InitTimer t = timer("create providers")) {
            try (InitTimer rt = timer("create MetaAccess provider")) {
                metaAccess = createMetaAccess(runtime);
            }
            try (InitTimer rt = timer("create RegisterConfig")) {
                regConfig = createRegisterConfig(config, target);
            }
            try (InitTimer rt = timer("create CodeCache provider")) {
                codeCache = createCodeCache(runtime, target, regConfig);
            }
            try (InitTimer rt = timer("create ConstantReflection provider")) {
                constantReflection = createConstantReflection(runtime);
            }
            try (InitTimer rt = timer("create StackIntrospection provider")) {
                stackIntrospection = new HotSpotStackIntrospection(runtime);
            }
        }
        try (InitTimer rt = timer("instantiate backend")) {
            return createBackend(metaAccess, codeCache, constantReflection, stackIntrospection);
        }
    }

    protected JVMCIBackend createBackend(HotSpotMetaAccessProvider metaAccess, HotSpotCodeCacheProvider codeCache, ConstantReflectionProvider constantReflection,
                    StackIntrospection stackIntrospection) {
        return new JVMCIBackend(metaAccess, codeCache, constantReflection, stackIntrospection);
    }
}
