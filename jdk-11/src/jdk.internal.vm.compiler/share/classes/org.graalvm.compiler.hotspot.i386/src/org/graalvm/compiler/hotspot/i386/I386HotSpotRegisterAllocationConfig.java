/*
 * Copyright (c) 2015, 2015, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.hotspot.i386;

import static jdk.vm.ci.i386.I386.r10;
import static jdk.vm.ci.i386.I386.r11;
import static jdk.vm.ci.i386.I386.r12;
import static jdk.vm.ci.i386.I386.r13;
import static jdk.vm.ci.i386.I386.r14;
import static jdk.vm.ci.i386.I386.r8;
import static jdk.vm.ci.i386.I386.r9;
import static jdk.vm.ci.i386.I386.rax;
import static jdk.vm.ci.i386.I386.rbp;
import static jdk.vm.ci.i386.I386.rbx;
import static jdk.vm.ci.i386.I386.rcx;
import static jdk.vm.ci.i386.I386.rdi;
import static jdk.vm.ci.i386.I386.rdx;
import static jdk.vm.ci.i386.I386.rsi;
import static jdk.vm.ci.i386.I386.xmm0;
import static jdk.vm.ci.i386.I386.xmm1;
import static jdk.vm.ci.i386.I386.xmm10;
import static jdk.vm.ci.i386.I386.xmm11;
import static jdk.vm.ci.i386.I386.xmm12;
import static jdk.vm.ci.i386.I386.xmm13;
import static jdk.vm.ci.i386.I386.xmm14;
import static jdk.vm.ci.i386.I386.xmm15;
import static jdk.vm.ci.i386.I386.xmm2;
import static jdk.vm.ci.i386.I386.xmm3;
import static jdk.vm.ci.i386.I386.xmm4;
import static jdk.vm.ci.i386.I386.xmm5;
import static jdk.vm.ci.i386.I386.xmm6;
import static jdk.vm.ci.i386.I386.xmm7;
import static jdk.vm.ci.i386.I386.xmm8;
import static jdk.vm.ci.i386.I386.xmm9;

import java.util.ArrayList;
import java.util.BitSet;

import org.graalvm.compiler.core.common.alloc.RegisterAllocationConfig;

import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.RegisterArray;
import jdk.vm.ci.code.RegisterConfig;

class I386HotSpotRegisterAllocationConfig extends RegisterAllocationConfig {
    /**
     * Specify priority of register selection within phases of register allocation. Highest priority
     * is first. A useful heuristic is to give registers a low priority when they are required by
     * machine instructions, like EAX and EDX on I486, and choose no-save registers before
     * save-on-call, & save-on-call before save-on-entry. Registers which participate in fixed
     * calling sequences should come last. Registers which are used as pairs must fall on an even
     * boundary.
     *
     * Adopted from x86_64.ad.
     */
    // @formatter:off
    static final Register[] registerAllocationOrder = {
        r10, r11, r8, r9, r12, rcx, rbx, rdi, rdx, rsi, rax, rbp, r13, r14, /*r15,*/ /*rsp,*/
        xmm0, xmm1, xmm2,  xmm3,  xmm4,  xmm5,  xmm6,  xmm7,
        xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
    };
    // @formatter:on

    I386HotSpotRegisterAllocationConfig(RegisterConfig registerConfig, String[] allocationRestrictedTo) {
        super(registerConfig, allocationRestrictedTo);
    }

    @Override
    protected RegisterArray initAllocatable(RegisterArray registers) {
        BitSet regMap = new BitSet(registerConfig.getAllocatableRegisters().size());
        for (Register reg : registers) {
            regMap.set(reg.number);
        }

        ArrayList<Register> allocatableRegisters = new ArrayList<>(registers.size());
        for (Register reg : registerAllocationOrder) {
            if (regMap.get(reg.number)) {
                allocatableRegisters.add(reg);
            }
        }

        return super.initAllocatable(new RegisterArray(allocatableRegisters));
    }
}
