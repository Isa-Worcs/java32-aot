#!/bin/sh


#rename 's/AMD64/I386/' src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.hotspot.i386/src/org/graalvm/compiler/hotspot/i386/AMD64*

#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.hotspot.i386/src/org/graalvm/compiler/hotspot/i386"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.lir.i386/src/org/graalvm/compiler/lir/i386"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.asm.i386/src/org/graalvm/compiler/asm/i386"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.lir.i386/src/org/graalvm/compiler/lir/i386/vector"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.lir.i386/src/org/graalvm/compiler/lir/i386/phases"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.core.i386/src/org/graalvm/compiler/core/i386"
#files="src/jdk.internal.vm.compiler/share/classes/org.graalvm.compiler.replacements.i386/src/org/graalvm/compiler/replacements/i386"

files="dummy"

for i in $files/*; do
    sed -i 's/AMD64/I386/g' $i
    sed -i 's/amd64/i386/g' $i
done

