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

import static org.graalvm.compiler.lir.LIRValueUtil.asJavaConstant;
import static org.graalvm.compiler.lir.LIRValueUtil.isJavaConstant;
import static jdk.vm.ci.i386.I386.rax;
import static jdk.vm.ci.i386.I386.rbx;
import static jdk.vm.ci.code.ValueUtil.asRegister;
import static jdk.vm.ci.code.ValueUtil.isRegister;

import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.debug.GraalError;
import org.graalvm.compiler.hotspot.HotSpotCounterOp;
import org.graalvm.compiler.hotspot.GraalHotSpotVMConfig;
import org.graalvm.compiler.hotspot.meta.HotSpotRegistersProvider;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.TargetDescription;
import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Value;

@Opcode("BenchMarkCounter")
public class I386HotSpotCounterOp extends HotSpotCounterOp {
    public static final LIRInstructionClass<I386HotSpotCounterOp> TYPE = LIRInstructionClass.create(I386HotSpotCounterOp.class);

    @Alive({OperandFlag.STACK, OperandFlag.UNINITIALIZED}) private AllocatableValue backupSlot;

    public I386HotSpotCounterOp(String name, String group, Value increment, HotSpotRegistersProvider registers, GraalHotSpotVMConfig config, AllocatableValue backupSlot) {
        super(TYPE, name, group, increment, registers, config);
        this.backupSlot = backupSlot;
    }

    public I386HotSpotCounterOp(String[] names, String[] groups, Value[] increments, HotSpotRegistersProvider registers, GraalHotSpotVMConfig config, AllocatableValue backupSlot) {
        super(TYPE, names, groups, increments, registers, config);
        this.backupSlot = backupSlot;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb) {
        I386MacroAssembler masm = (I386MacroAssembler) crb.asm;
        TargetDescription target = crb.target;

        Register scratch;
        // It can happen that the rax register is the increment register, in this case we do not
        // want to spill it to the stack.
        if (!contains(increments, rax)) {
            scratch = rax;
        } else if (!contains(increments, rbx)) {
            scratch = rbx;
        } else {
            // In this case rax and rbx are used as increment. Either we implement a third register
            // or we implement a spillover the value from rax to rbx or vice versa during
            // emitIncrement().
            throw GraalError.unimplemented("RAX and RBX are increment registers at the same time, spilling over the scratch register is not supported right now");
        }

        // address for counters array
        I386Address countersArrayAddr = new I386Address(thread, config.jvmciCountersThreadOffset);
        Register countersArrayReg = scratch;

        // backup scratch register
        masm.movq((I386Address) crb.asAddress(backupSlot), scratch);

        // load counters array
        masm.movptr(countersArrayReg, countersArrayAddr);
        CounterProcedure emitProcedure = (counterIndex, increment, displacement) -> emitIncrement(masm, countersArrayReg, increment, displacement);
        forEachCounter(emitProcedure, target);

        // restore scratch register
        masm.movq(scratch, (I386Address) crb.asAddress(backupSlot));
    }

    /**
     * Tests if the array contains the register.
     */
    private static boolean contains(Value[] increments, Register register) {
        for (Value increment : increments) {
            if (isRegister(increment) && asRegister(increment).equals(register)) {
                return true;
            }
        }
        return false;
    }

    private static void emitIncrement(I386MacroAssembler masm, Register countersArrayReg, Value incrementValue, int displacement) {
        // address for counter value
        I386Address counterAddr = new I386Address(countersArrayReg, displacement);
        // increment counter (in memory)
        if (isJavaConstant(incrementValue)) {
            int increment = asInt(asJavaConstant(incrementValue));
            masm.incrementq(counterAddr, increment);
        } else {
            masm.addq(counterAddr, asRegister(incrementValue));
        }

    }
}
