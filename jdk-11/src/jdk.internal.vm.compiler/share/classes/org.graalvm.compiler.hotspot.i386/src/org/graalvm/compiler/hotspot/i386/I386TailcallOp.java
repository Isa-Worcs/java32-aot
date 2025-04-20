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
package org.graalvm.compiler.hotspot.i386;

import static jdk.vm.ci.code.ValueUtil.asRegister;

import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.i386.I386LIRInstruction;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.code.InstalledCode;
import jdk.vm.ci.meta.Value;

/**
 * Performs a hard-coded tail call to the specified target, which normally should be an
 * {@link InstalledCode} instance.
 */
@Opcode("TAILCALL")
public final class I386TailcallOp extends I386LIRInstruction {
    public static final LIRInstructionClass<I386TailcallOp> TYPE = LIRInstructionClass.create(I386TailcallOp.class);

    @Use protected Value target;
    @Alive protected Value[] parameters;

    public I386TailcallOp(Value[] parameters, Value target) {
        super(TYPE);
        this.target = target;
        this.parameters = parameters;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        // destroy the current frame (now the return address is the top of stack)
        masm.leave();

        // jump to the target method
        masm.jmp(asRegister(target));
        masm.ensureUniquePC();
    }
}
