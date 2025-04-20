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
package org.graalvm.compiler.lir.i386;

import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.meta.JavaConstant;

/**
 * Writes well known garbage values to stack slots.
 */
@Opcode("ZAP_STACK")
public final class I386ZapStackOp extends I386LIRInstruction {
    public static final LIRInstructionClass<I386ZapStackOp> TYPE = LIRInstructionClass.create(I386ZapStackOp.class);

    /**
     * The stack slots that are zapped.
     */
    @Def(OperandFlag.STACK) protected final StackSlot[] zappedStack;

    /**
     * The garbage values that are written to the stack.
     */
    protected final JavaConstant[] zapValues;

    public I386ZapStackOp(StackSlot[] zappedStack, JavaConstant[] zapValues) {
        super(TYPE);
        this.zappedStack = zappedStack;
        this.zapValues = zapValues;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        for (int i = 0; i < zappedStack.length; i++) {
            StackSlot slot = zappedStack[i];
            if (slot != null) {
                I386Move.const2stack(crb, masm, slot, zapValues[i]);
            }
        }
    }
}
