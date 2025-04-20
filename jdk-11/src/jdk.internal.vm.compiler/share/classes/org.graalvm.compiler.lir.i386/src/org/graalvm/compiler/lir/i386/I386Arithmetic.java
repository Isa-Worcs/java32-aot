/*
 * Copyright (c) 2011, 2015, Oracle and/or its affiliates. All rights reserved.
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

import static jdk.vm.ci.code.ValueUtil.asRegister;

import org.graalvm.compiler.asm.Label;
import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386Assembler.ConditionFlag;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.core.common.LIRKind;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.meta.AllocatableValue;

public enum I386Arithmetic {
    FREM,
    DREM;

    public static class FPDivRemOp extends I386LIRInstruction {
        public static final LIRInstructionClass<FPDivRemOp> TYPE = LIRInstructionClass.create(FPDivRemOp.class);

        @Opcode private final I386Arithmetic opcode;
        @Def protected AllocatableValue result;
        @Use protected AllocatableValue x;
        @Use protected AllocatableValue y;
        @Temp protected AllocatableValue raxTemp;

        public FPDivRemOp(I386Arithmetic opcode, AllocatableValue result, AllocatableValue x, AllocatableValue y) {
            super(TYPE);
            this.opcode = opcode;
            this.result = result;
            this.raxTemp = I386.rax.asValue(LIRKind.value(I386Kind.DWORD));
            this.x = x;
            this.y = y;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386Address tmp = new I386Address(I386.rsp);
            masm.subq(I386.rsp, 8);
            if (opcode == FREM) {
                masm.movflt(tmp, asRegister(y));
                masm.flds(tmp);
                masm.movflt(tmp, asRegister(x));
                masm.flds(tmp);
            } else {
                assert opcode == DREM;
                masm.movdbl(tmp, asRegister(y));
                masm.fldd(tmp);
                masm.movdbl(tmp, asRegister(x));
                masm.fldd(tmp);
            }

            Label label = new Label();
            masm.bind(label);
            masm.fprem();
            masm.fwait();
            masm.fnstswAX();
            masm.testl(I386.rax, 0x400);
            masm.jcc(ConditionFlag.NotZero, label);
            masm.fxch(1);
            masm.fpop();

            if (opcode == FREM) {
                masm.fstps(tmp);
                masm.movflt(asRegister(result), tmp);
            } else {
                masm.fstpd(tmp);
                masm.movdbl(asRegister(result), tmp);
            }
            masm.addq(I386.rsp, 8);
        }

        @Override
        public void verify() {
            super.verify();
            assert (opcode.name().startsWith("F") && result.getPlatformKind() == I386Kind.SINGLE && x.getPlatformKind() == I386Kind.SINGLE && y.getPlatformKind() == I386Kind.SINGLE) ||
                            (opcode.name().startsWith("D") && result.getPlatformKind() == I386Kind.DOUBLE && x.getPlatformKind() == I386Kind.DOUBLE && y.getPlatformKind() == I386Kind.DOUBLE);
        }
    }
}
