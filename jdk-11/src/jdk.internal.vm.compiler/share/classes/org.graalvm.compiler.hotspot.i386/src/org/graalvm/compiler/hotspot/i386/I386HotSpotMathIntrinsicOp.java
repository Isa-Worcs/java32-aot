/*
 * Copyright (c) 2011, 2016, Oracle and/or its affiliates. All rights reserved.
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
import org.graalvm.compiler.debug.GraalError;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.i386.I386LIRInstruction;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.meta.Value;

/**
 * This provides the default implementation expected by some HotSpot based lowerings of Math
 * intrinsics. Depending on the release different patterns might be used.
 */
public final class I386HotSpotMathIntrinsicOp extends I386LIRInstruction {
    public static final LIRInstructionClass<I386HotSpotMathIntrinsicOp> TYPE = LIRInstructionClass.create(I386HotSpotMathIntrinsicOp.class);

    public enum IntrinsicOpcode {
        SIN,
        COS,
        TAN,
        LOG,
        LOG10
    }

    @Opcode private final IntrinsicOpcode opcode;
    @Def protected Value result;
    @Use protected Value input;

    public I386HotSpotMathIntrinsicOp(IntrinsicOpcode opcode, Value result, Value input) {
        super(TYPE);
        this.opcode = opcode;
        this.result = result;
        this.input = input;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        switch (opcode) {
            case LOG:
                masm.flog(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE), false);
                break;
            case LOG10:
                masm.flog(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE), true);
                break;
            case SIN:
                masm.fsin(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE));
                break;
            case COS:
                masm.fcos(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE));
                break;
            case TAN:
                masm.ftan(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE));
                break;
            default:
                throw GraalError.shouldNotReachHere();
        }
    }
}
