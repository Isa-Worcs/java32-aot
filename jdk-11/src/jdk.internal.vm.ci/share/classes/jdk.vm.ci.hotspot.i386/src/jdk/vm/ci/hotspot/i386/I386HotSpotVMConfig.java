/*
 * Copyright (c) 2016, Oracle and/or its affiliates. All rights reserved.
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

import jdk.vm.ci.hotspot.HotSpotVMConfigAccess;
import jdk.vm.ci.hotspot.HotSpotVMConfigStore;

class I386HotSpotVMConfig extends HotSpotVMConfigAccess {

    I386HotSpotVMConfig(HotSpotVMConfigStore config) {
        super(config);
    }

    final boolean windowsOs = System.getProperty("os.name", "").startsWith("Windows");

    final boolean useCountLeadingZerosInstruction = getFlag("UseCountLeadingZerosInstruction", Boolean.class);
    final boolean useCountTrailingZerosInstruction = getFlag("UseCountTrailingZerosInstruction", Boolean.class);
    final boolean useCompressedOops = false;

    // CPU capabilities
    final int useSSE = getFlag("UseSSE", Integer.class);
    final int useAVX = getFlag("UseAVX", Integer.class);

    final long vmVersionFeatures = getFieldValue("Abstract_VM_Version::_features", Long.class, "uint64_t");

    // CPU feature flags
    final long i386CX8 = getConstant("VM_Version::CPU_CX8", Long.class);
    final long i386CMOV = getConstant("VM_Version::CPU_CMOV", Long.class);
    final long i386FXSR = getConstant("VM_Version::CPU_FXSR", Long.class);
    final long i386HT = getConstant("VM_Version::CPU_HT", Long.class);
    final long i386MMX = getConstant("VM_Version::CPU_MMX", Long.class);
    final long i3863DNOWPREFETCH = getConstant("VM_Version::CPU_3DNOW_PREFETCH", Long.class);
    final long i386SSE = getConstant("VM_Version::CPU_SSE", Long.class);
    final long i386SSE2 = getConstant("VM_Version::CPU_SSE2", Long.class);
    final long i386SSE3 = getConstant("VM_Version::CPU_SSE3", Long.class);
    final long i386SSSE3 = getConstant("VM_Version::CPU_SSSE3", Long.class);
    final long i386SSE4A = getConstant("VM_Version::CPU_SSE4A", Long.class);
    final long i386SSE41 = getConstant("VM_Version::CPU_SSE4_1", Long.class);
    final long i386SSE42 = getConstant("VM_Version::CPU_SSE4_2", Long.class);
    final long i386POPCNT = getConstant("VM_Version::CPU_POPCNT", Long.class);
    final long i386LZCNT = getConstant("VM_Version::CPU_LZCNT", Long.class);
    final long i386TSC = getConstant("VM_Version::CPU_TSC", Long.class);
    final long i386TSCINV = getConstant("VM_Version::CPU_TSCINV", Long.class);
    final long i386AVX = getConstant("VM_Version::CPU_AVX", Long.class);
    final long i386AVX2 = getConstant("VM_Version::CPU_AVX2", Long.class);
    final long i386AES = getConstant("VM_Version::CPU_AES", Long.class);
    final long i386ERMS = getConstant("VM_Version::CPU_ERMS", Long.class);
    final long i386CLMUL = getConstant("VM_Version::CPU_CLMUL", Long.class);
    final long i386BMI1 = getConstant("VM_Version::CPU_BMI1", Long.class);
    final long i386BMI2 = getConstant("VM_Version::CPU_BMI2", Long.class);
    final long i386RTM = getConstant("VM_Version::CPU_RTM", Long.class);
    final long i386ADX = getConstant("VM_Version::CPU_ADX", Long.class);
    final long i386AVX512F = getConstant("VM_Version::CPU_AVX512F", Long.class);
    final long i386AVX512DQ = getConstant("VM_Version::CPU_AVX512DQ", Long.class);
    final long i386AVX512PF = getConstant("VM_Version::CPU_AVX512PF", Long.class);
    final long i386AVX512ER = getConstant("VM_Version::CPU_AVX512ER", Long.class);
    final long i386AVX512CD = getConstant("VM_Version::CPU_AVX512CD", Long.class);
    final long i386AVX512BW = getConstant("VM_Version::CPU_AVX512BW", Long.class);
    final long i386AVX512VL = getConstant("VM_Version::CPU_AVX512VL", Long.class);
    final long i386SHA = getConstant("VM_Version::CPU_SHA", Long.class);
    final long i386FMA = getConstant("VM_Version::CPU_FMA", Long.class);
}
