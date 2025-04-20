/*
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved.
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

import static jdk.vm.ci.i386.I386.rdx;

import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.StandardOp.SaveRegistersOp;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;
import org.graalvm.compiler.lir.framemap.FrameMap;

import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.RegisterConfig;
import jdk.vm.ci.code.RegisterSaveLayout;
import jdk.vm.ci.meta.JavaKind;

/**
 * Pops the current frame off the stack including the return address and restores the return
 * registers stored on the stack.
 */
@Opcode("LEAVE_CURRENT_STACK_FRAME")
final class I386HotSpotLeaveCurrentStackFrameOp extends I386HotSpotEpilogueOp {

    public static final LIRInstructionClass<I386HotSpotLeaveCurrentStackFrameOp> TYPE = LIRInstructionClass.create(I386HotSpotLeaveCurrentStackFrameOp.class);

    private final SaveRegistersOp saveRegisterOp;

    I386HotSpotLeaveCurrentStackFrameOp(SaveRegistersOp saveRegisterOp) {
        super(TYPE);
        this.saveRegisterOp = saveRegisterOp;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        FrameMap frameMap = crb.frameMap;
        RegisterConfig registerConfig = frameMap.getRegisterConfig();
        RegisterSaveLayout registerSaveLayout = saveRegisterOp.getMap(frameMap);
        Register stackPointer = registerConfig.getFrameRegister();

        // Restore integer result register.
        final int stackSlotSize = frameMap.getTarget().wordSize;
        Register integerResultRegister = registerConfig.getReturnRegister(JavaKind.Long);
        masm.movptr(integerResultRegister, new I386Address(stackPointer, registerSaveLayout.registerToSlot(integerResultRegister) * stackSlotSize));
        masm.movptr(rdx, new I386Address(stackPointer, registerSaveLayout.registerToSlot(rdx) * stackSlotSize));

        // Restore float result register.
        Register floatResultRegister = registerConfig.getReturnRegister(JavaKind.Double);
        masm.movdbl(floatResultRegister, new I386Address(stackPointer, registerSaveLayout.registerToSlot(floatResultRegister) * stackSlotSize));

        /*
         * All of the register save area will be popped of the stack. Only the return address
         * remains.
         */
        leaveFrameAndRestoreRbp(crb, masm);

        // Remove return address.
        masm.addq(stackPointer, crb.target.arch.getReturnAddressSize());
    }
}
