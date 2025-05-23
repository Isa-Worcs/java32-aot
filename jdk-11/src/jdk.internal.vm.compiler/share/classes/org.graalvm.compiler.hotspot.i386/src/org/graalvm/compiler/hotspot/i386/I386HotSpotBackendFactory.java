/*
 * Copyright (c) 2012, 2016, Oracle and/or its affiliates. All rights reserved.
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

import static jdk.vm.ci.common.InitTimer.timer;
import static org.graalvm.compiler.hotspot.HotSpotBackend.Options.GraalArithmeticStubs;
import java.util.ArrayList;
import java.util.List;

import org.graalvm.compiler.api.replacements.SnippetReflectionProvider;
import org.graalvm.compiler.bytecode.BytecodeProvider;
import org.graalvm.compiler.core.common.spi.ConstantFieldProvider;
import org.graalvm.compiler.hotspot.GraalHotSpotVMConfig;
import org.graalvm.compiler.hotspot.HotSpotBackend;
import org.graalvm.compiler.hotspot.HotSpotBackendFactory;
import org.graalvm.compiler.hotspot.HotSpotGraalRuntimeProvider;
import org.graalvm.compiler.hotspot.HotSpotReplacementsImpl;
import org.graalvm.compiler.hotspot.meta.AddressLoweringHotSpotSuitesProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotForeignCallsProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotGraalConstantFieldProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotGraphBuilderPlugins;
import org.graalvm.compiler.hotspot.meta.HotSpotHostForeignCallsProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotLoweringProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotProviders;
import org.graalvm.compiler.hotspot.meta.HotSpotRegisters;
import org.graalvm.compiler.hotspot.meta.HotSpotRegistersProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotSnippetReflectionProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotStampProvider;
import org.graalvm.compiler.hotspot.meta.HotSpotSuitesProvider;
import org.graalvm.compiler.hotspot.word.HotSpotWordTypes;
import org.graalvm.compiler.nodes.graphbuilderconf.GraphBuilderConfiguration.Plugins;
import org.graalvm.compiler.nodes.spi.LoweringProvider;
import org.graalvm.compiler.nodes.spi.Replacements;
import org.graalvm.compiler.options.OptionValues;
import org.graalvm.compiler.phases.common.AddressLoweringPhase;
import org.graalvm.compiler.phases.tiers.CompilerConfiguration;
import org.graalvm.compiler.phases.util.Providers;
import org.graalvm.compiler.replacements.i386.I386GraphBuilderPlugins;
import org.graalvm.compiler.replacements.classfile.ClassfileBytecodeProvider;
import org.graalvm.compiler.serviceprovider.ServiceProvider;
import org.graalvm.compiler.word.WordTypes;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.code.Architecture;
import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.RegisterConfig;
import jdk.vm.ci.code.TargetDescription;
import jdk.vm.ci.common.InitTimer;
import jdk.vm.ci.hotspot.HotSpotCodeCacheProvider;
import jdk.vm.ci.hotspot.HotSpotConstantReflectionProvider;
import jdk.vm.ci.hotspot.HotSpotJVMCIRuntimeProvider;
import jdk.vm.ci.hotspot.HotSpotMetaAccessProvider;
import jdk.vm.ci.meta.Value;
import jdk.vm.ci.runtime.JVMCIBackend;

@ServiceProvider(HotSpotBackendFactory.class)
public class I386HotSpotBackendFactory implements HotSpotBackendFactory {

    @Override
    public String getName() {
        return "community";
    }

    @Override
    public Class<? extends Architecture> getArchitecture() {
        return I386.class;
    }

    @Override
    @SuppressWarnings("try")
    public HotSpotBackend createBackend(HotSpotGraalRuntimeProvider graalRuntime, CompilerConfiguration compilerConfiguration, HotSpotJVMCIRuntimeProvider jvmciRuntime, HotSpotBackend host) {
        assert host == null;

        OptionValues options = graalRuntime.getOptions();
        JVMCIBackend jvmci = jvmciRuntime.getHostJVMCIBackend();
        GraalHotSpotVMConfig config = graalRuntime.getVMConfig();
        HotSpotProviders providers;
        HotSpotRegistersProvider registers;
        HotSpotCodeCacheProvider codeCache = (HotSpotCodeCacheProvider) jvmci.getCodeCache();
        TargetDescription target = codeCache.getTarget();
        HotSpotHostForeignCallsProvider foreignCalls;
        Value[] nativeABICallerSaveRegisters;
        HotSpotMetaAccessProvider metaAccess = (HotSpotMetaAccessProvider) jvmci.getMetaAccess();
        HotSpotConstantReflectionProvider constantReflection = (HotSpotConstantReflectionProvider) jvmci.getConstantReflection();
        ConstantFieldProvider constantFieldProvider = new HotSpotGraalConstantFieldProvider(config, metaAccess);
        HotSpotLoweringProvider lowerer;
        HotSpotSnippetReflectionProvider snippetReflection;
        HotSpotReplacementsImpl replacements;
        HotSpotSuitesProvider suites;
        HotSpotWordTypes wordTypes;
        Plugins plugins;
        BytecodeProvider bytecodeProvider;
        try (InitTimer t = timer("create providers")) {
            try (InitTimer rt = timer("create HotSpotRegisters provider")) {
                registers = createRegisters();
            }
            try (InitTimer rt = timer("create NativeABICallerSaveRegisters")) {
                nativeABICallerSaveRegisters = createNativeABICallerSaveRegisters(config, codeCache.getRegisterConfig());
            }
            try (InitTimer rt = timer("create WordTypes")) {
                wordTypes = new HotSpotWordTypes(metaAccess, target.wordJavaKind);
            }
            try (InitTimer rt = timer("create ForeignCalls provider")) {
                foreignCalls = createForeignCalls(jvmciRuntime, graalRuntime, metaAccess, codeCache, wordTypes, nativeABICallerSaveRegisters);
            }
            try (InitTimer rt = timer("create Lowerer provider")) {
                lowerer = createLowerer(graalRuntime, metaAccess, foreignCalls, registers, constantReflection, target);
            }
            HotSpotStampProvider stampProvider = new HotSpotStampProvider();
            Providers p = new Providers(metaAccess, codeCache, constantReflection, constantFieldProvider, foreignCalls, lowerer, null, stampProvider);

            try (InitTimer rt = timer("create SnippetReflection provider")) {
                snippetReflection = createSnippetReflection(graalRuntime, constantReflection, wordTypes);
            }
            try (InitTimer rt = timer("create Bytecode provider")) {
                bytecodeProvider = new ClassfileBytecodeProvider(metaAccess, snippetReflection);
            }
            try (InitTimer rt = timer("create Replacements provider")) {
                replacements = createReplacements(options, p, snippetReflection, bytecodeProvider);
            }
            try (InitTimer rt = timer("create GraphBuilderPhase plugins")) {
                plugins = createGraphBuilderPlugins(compilerConfiguration, config, options, target, constantReflection, foreignCalls, lowerer, metaAccess, snippetReflection, replacements, wordTypes,
                                stampProvider);
                replacements.setGraphBuilderPlugins(plugins);
            }
            try (InitTimer rt = timer("create Suites provider")) {
                suites = createSuites(config, graalRuntime, compilerConfiguration, plugins, registers, replacements, options);
            }
            providers = new HotSpotProviders(metaAccess, codeCache, constantReflection, constantFieldProvider, foreignCalls, lowerer, replacements, suites, registers,
                            snippetReflection, wordTypes,
                            plugins);
        }
        try (InitTimer rt = timer("instantiate backend")) {
            return createBackend(config, graalRuntime, providers);
        }
    }

    protected Plugins createGraphBuilderPlugins(CompilerConfiguration compilerConfiguration, GraalHotSpotVMConfig config, OptionValues options, TargetDescription target,
                    HotSpotConstantReflectionProvider constantReflection, HotSpotHostForeignCallsProvider foreignCalls, LoweringProvider lowerer, HotSpotMetaAccessProvider metaAccess,
                    HotSpotSnippetReflectionProvider snippetReflection, HotSpotReplacementsImpl replacements, HotSpotWordTypes wordTypes, HotSpotStampProvider stampProvider) {
        Plugins plugins = HotSpotGraphBuilderPlugins.create(compilerConfiguration, config, wordTypes, metaAccess, constantReflection, snippetReflection, foreignCalls, lowerer, stampProvider,
                        replacements);
        I386GraphBuilderPlugins.register(plugins, replacements.getDefaultReplacementBytecodeProvider(), (I386) target.arch, GraalArithmeticStubs.getValue(options));
        return plugins;
    }

    protected I386HotSpotBackend createBackend(GraalHotSpotVMConfig config, HotSpotGraalRuntimeProvider runtime, HotSpotProviders providers) {
        return new I386HotSpotBackend(config, runtime, providers);
    }

    protected HotSpotRegistersProvider createRegisters() {
        return new HotSpotRegisters(I386.r15, I386.r12, I386.rsp);
    }

    protected HotSpotReplacementsImpl createReplacements(OptionValues options, Providers p, SnippetReflectionProvider snippetReflection, BytecodeProvider bytecodeProvider) {
        return new HotSpotReplacementsImpl(options, p, snippetReflection, bytecodeProvider, p.getCodeCache().getTarget());
    }

    protected I386HotSpotForeignCallsProvider createForeignCalls(HotSpotJVMCIRuntimeProvider jvmciRuntime, HotSpotGraalRuntimeProvider runtime, HotSpotMetaAccessProvider metaAccess,
                    HotSpotCodeCacheProvider codeCache, WordTypes wordTypes, Value[] nativeABICallerSaveRegisters) {
        return new I386HotSpotForeignCallsProvider(jvmciRuntime, runtime, metaAccess, codeCache, wordTypes, nativeABICallerSaveRegisters);
    }

    /**
     * @param replacements
     */
    protected HotSpotSuitesProvider createSuites(GraalHotSpotVMConfig config, HotSpotGraalRuntimeProvider runtime, CompilerConfiguration compilerConfiguration, Plugins plugins,
                    HotSpotRegistersProvider registers, Replacements replacements, OptionValues options) {
        return new AddressLoweringHotSpotSuitesProvider(new I386HotSpotSuitesCreator(compilerConfiguration, plugins), config, runtime,
                        new AddressLoweringPhase(new I386HotSpotAddressLowering(config, registers.getHeapBaseRegister(), options)));
    }

    protected HotSpotSnippetReflectionProvider createSnippetReflection(HotSpotGraalRuntimeProvider runtime, HotSpotConstantReflectionProvider constantReflection, WordTypes wordTypes) {
        return new HotSpotSnippetReflectionProvider(runtime, constantReflection, wordTypes);
    }

    protected HotSpotLoweringProvider createLowerer(HotSpotGraalRuntimeProvider runtime, HotSpotMetaAccessProvider metaAccess, HotSpotForeignCallsProvider foreignCalls,
                    HotSpotRegistersProvider registers, HotSpotConstantReflectionProvider constantReflection, TargetDescription target) {
        return new I386HotSpotLoweringProvider(runtime, metaAccess, foreignCalls, registers, constantReflection, target);
    }

    protected Value[] createNativeABICallerSaveRegisters(GraalHotSpotVMConfig config, RegisterConfig regConfig) {
        List<Register> callerSave = new ArrayList<>(regConfig.getAllocatableRegisters().asList());
        if (config.windowsOs) {
            // http://msdn.microsoft.com/en-us/library/9z1stfyw.aspx
            callerSave.remove(I386.rdi);
            callerSave.remove(I386.rsi);
            callerSave.remove(I386.rbx);
            callerSave.remove(I386.rbp);
            callerSave.remove(I386.rsp);
            callerSave.remove(I386.r12);
            callerSave.remove(I386.r13);
            callerSave.remove(I386.r14);
            callerSave.remove(I386.r15);
            callerSave.remove(I386.xmm6);
            callerSave.remove(I386.xmm7);
            callerSave.remove(I386.xmm8);
            callerSave.remove(I386.xmm9);
            callerSave.remove(I386.xmm10);
            callerSave.remove(I386.xmm11);
            callerSave.remove(I386.xmm12);
            callerSave.remove(I386.xmm13);
            callerSave.remove(I386.xmm14);
            callerSave.remove(I386.xmm15);
        } else {
            /*
             * System V Application Binary Interface, I386 Architecture Processor Supplement
             *
             * Draft Version 0.96
             *
             * http://www.uclibc.org/docs/psABI-x86_64.pdf
             *
             * 3.2.1
             *
             * ...
             *
             * This subsection discusses usage of each register. Registers %rbp, %rbx and %r12
             * through %r15 "belong" to the calling function and the called function is required to
             * preserve their values. In other words, a called function must preserve these
             * registers' values for its caller. Remaining registers "belong" to the called
             * function. If a calling function wants to preserve such a register value across a
             * function call, it must save the value in its local stack frame.
             */
            callerSave.remove(I386.rbp);
            callerSave.remove(I386.rbx);
            callerSave.remove(I386.r12);
            callerSave.remove(I386.r13);
            callerSave.remove(I386.r14);
            callerSave.remove(I386.r15);
        }
        Value[] nativeABICallerSaveRegisters = new Value[callerSave.size()];
        for (int i = 0; i < callerSave.size(); i++) {
            nativeABICallerSaveRegisters[i] = callerSave.get(i).asValue();
        }
        return nativeABICallerSaveRegisters;
    }

    @Override
    public String toString() {
        return "I386";
    }
}
