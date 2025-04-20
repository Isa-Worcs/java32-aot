/*
 * Copyright (c) 2013, 2018, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.lir.i386.vector;

import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.asm.i386.I386VectorAssembler;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.i386.I386LIRInstruction;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

public abstract class I386VectorLIRInstruction extends I386LIRInstruction {
    public static final LIRInstructionClass<I386VectorLIRInstruction> TYPE = LIRInstructionClass.create(I386VectorLIRInstruction.class);

    protected I386VectorLIRInstruction(LIRInstructionClass<? extends I386LIRInstruction> c) {
        super(c);
    }

    @Override
    public final void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        emitCode(crb, (I386VectorAssembler) masm);
    }

    public abstract void emitCode(CompilationResultBuilder crb, I386VectorAssembler vasm);
}
