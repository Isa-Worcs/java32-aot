/*
 * Copyright (c) 2015, 2016, Oracle and/or its affiliates. All rights reserved.
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

package org.graalvm.compiler.core.i386;

import static jdk.vm.ci.code.ValueUtil.isRegister;
import static org.graalvm.compiler.lir.LIRValueUtil.asConstant;
import static org.graalvm.compiler.lir.LIRValueUtil.isConstantValue;
import static org.graalvm.compiler.lir.LIRValueUtil.isStackSlotValue;

import org.graalvm.compiler.asm.i386.I386Assembler;
import org.graalvm.compiler.core.common.NumUtil;
import org.graalvm.compiler.core.common.type.DataPointerConstant;
import org.graalvm.compiler.debug.GraalError;
import org.graalvm.compiler.lir.LIRInstruction;
import org.graalvm.compiler.lir.i386.I386AddressValue;
import org.graalvm.compiler.lir.i386.I386LIRInstruction;
import org.graalvm.compiler.lir.i386.I386Move;
import org.graalvm.compiler.lir.i386.I386Move.I386StackMove;
import org.graalvm.compiler.lir.i386.I386Move.LeaDataOp;
import org.graalvm.compiler.lir.i386.I386Move.LeaOp;
import org.graalvm.compiler.lir.i386.I386Move.MoveFromConstOp;
import org.graalvm.compiler.lir.i386.I386Move.MoveFromRegOp;
import org.graalvm.compiler.lir.i386.I386Move.MoveToRegOp;

import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.code.Register;
import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Constant;
import jdk.vm.ci.meta.JavaConstant;
import jdk.vm.ci.meta.Value;

public abstract class I386MoveFactory extends I386MoveFactoryBase {

    public I386MoveFactory(BackupSlotProvider backupSlotProvider) {
        super(backupSlotProvider);
    }

    @Override
    public boolean canInlineConstant(Constant con) {
        if (con instanceof JavaConstant) {
            JavaConstant c = (JavaConstant) con;
            switch (c.getJavaKind()) {
                case Long:
                    return NumUtil.isInt(c.asLong());
                case Object:
                    return c.isNull();
                default:
                    return true;
            }
        }
        return false;
    }

    @Override
    public boolean allowConstantToStackMove(Constant constant) {
        if (constant instanceof DataPointerConstant) {
            return false;
        }
        if (constant instanceof JavaConstant && !I386Move.canMoveConst2Stack(((JavaConstant) constant))) {
            return false;
        }
        return true;
    }

    @Override
    public I386LIRInstruction createMove(AllocatableValue dst, Value src) {
        if (src instanceof I386AddressValue) {
            return new LeaOp(dst, (I386AddressValue) src, I386Assembler.OperandSize.QWORD);
        } else if (isConstantValue(src)) {
            return createLoad(dst, asConstant(src));
        } else if (isRegister(src) || isStackSlotValue(dst)) {
            return new MoveFromRegOp((I386Kind) dst.getPlatformKind(), dst, (AllocatableValue) src);
        } else {
            return new MoveToRegOp((I386Kind) dst.getPlatformKind(), dst, (AllocatableValue) src);
        }
    }

    @Override
    public I386LIRInstruction createStackMove(AllocatableValue result, AllocatableValue input, Register scratchRegister, AllocatableValue backupSlot) {
        return new I386StackMove(result, input, scratchRegister, backupSlot);
    }

    @Override
    public I386LIRInstruction createLoad(AllocatableValue dst, Constant src) {
        if (src instanceof JavaConstant) {
            return new MoveFromConstOp(dst, (JavaConstant) src);
        } else if (src instanceof DataPointerConstant) {
            return new LeaDataOp(dst, (DataPointerConstant) src);
        } else {
            throw GraalError.shouldNotReachHere(String.format("unsupported constant: %s", src));
        }
    }

    @Override
    public LIRInstruction createStackLoad(AllocatableValue result, Constant input) {
        if (input instanceof JavaConstant) {
            return new MoveFromConstOp(result, (JavaConstant) input);
        } else {
            throw GraalError.shouldNotReachHere(String.format("unsupported constant for stack load: %s", input));
        }
    }
}
