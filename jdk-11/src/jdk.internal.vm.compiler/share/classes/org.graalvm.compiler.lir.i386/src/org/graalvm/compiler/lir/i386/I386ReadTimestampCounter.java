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
package org.graalvm.compiler.lir.i386;

import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import jdk.vm.ci.i386.I386;
import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.meta.AllocatableValue;

import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.core.common.LIRKind;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

/**
 * I386 rdtsc operation. The result is in EDX:EAX.
 */
@Opcode("RDTSC")
public class I386ReadTimestampCounter extends I386LIRInstruction {
    public static final LIRInstructionClass<I386ReadTimestampCounter> TYPE = LIRInstructionClass.create(I386ReadTimestampCounter.class);

    @Def({REG}) protected AllocatableValue highResult;
    @Def({REG}) protected AllocatableValue lowResult;

    public I386ReadTimestampCounter() {
        super(TYPE);

        this.highResult = I386.rdx.asValue(LIRKind.value(I386Kind.DWORD));
        this.lowResult = I386.rax.asValue(LIRKind.value(I386Kind.DWORD));
    }

    public AllocatableValue getHighResult() {
        return highResult;
    }

    public AllocatableValue getLowResult() {
        return lowResult;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        masm.rdtsc();
    }
}
