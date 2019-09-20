// This patch allows to experiment the Common Lisp library cl-cycle in the SuperCollider context.
// It is obvious that for real time interaction, the needed algorithm(s) has/have to be re-writed at least as a SuperCollider function.

/*
<lispFunctionNameAsString>  <argument(s)Type>
--------------------------------------------------------------------------------
"KAPREKAR-CONSTANT"         integerArray      [optional] workingBase
"RHYTHMIC-SIEVE"            integerArray      [key] fieldX inFieldX optimize i j
"PEA-PATTERN"               integerArray
"SYMMETRIC-GROUP"           integerArraySerie
"SYMMETRIC-PERMUTATION"     anyArray integerArraySerie
"CIRCULAR-PERMUTATION"      integerArray initialBase circularBase
"LORENZ-DISCRETISATION"     float(1)
"COLLATZ-CONJECTURE"        integer
"INTERLACE-CYCLE"                             [rest] motifs
--------------------------------------------------------------------------------
"10->N"                     integer outputBase
"N->10"                     integerArray inputBase
"MK-INTEGER-LST"            anyArray
"FILL-DIGIT-LST"            integerArray length
--------------------------------------------------------------------------------
(1) Stricly superior to zero and stricly inferior to one.
Usage:
~cl_cycle.value(<globalVariableNameAsString>, <lispFunctionNameAsString>, <argument(s)Array>);
*/

(
~lispPackagePath = "~/lisp/packages/cl-cycle/".standardizePath;

// just in case ...
~stopLoopTest = false;

~pidRunningTest = {
	| pid |
	if(pid.pidRunning && ~stopLoopTest.not,
		{~pidRunningTest.value(pid)});
};

~cl_cycle = {
	| varName, funString, argAr |
	var outFile, pid;
	outFile = File(~lispPackagePath+/+"sclisp/.tmp_array","w");
	outFile.write(varName.asString);
	outFile.write("\n");
	outFile.write(funString.asString);
	outFile.write("\n");
	outFile.write(argAr.asCompileString);
	outFile.close;
	pid = (~lispPackagePath+/+"sclisp/cl-cycle.sh").unixCmd(postOutput:false);
	~pidRunningTest.value(pid);
	if(File.exists(~lispPackagePath+/+"sclisp/.tmp.scd"),
		{
			thisProcess.interpreter.executeFile(~lispPackagePath+/+"sclisp/.tmp.scd");
			File.delete(~lispPackagePath+/+"sclisp/.tmp.scd");
			varName.interpret;},
		{
			"WARNING: Please install sbcl [ http://www.sbcl.org/ ]".postln;
			postf("or create a symbolic link as follow:\nln -s /full/path/of/your/bin/sbcl %sbcl", ~lispPackagePath);
			"";
	})
};
)
/*
// instance 1
~cl_cycle.value("~foo", "KAPREKAR-CONSTANT", [Array.rand(4, 0, 11), 12]);
~foo = ~foo.clipExtend(2);
a = Pseq([Pbind(\degree, Pseq(~foo[0].flat, 1), \dur, Pseq((~foo[0]+1).flat*0.2, 1)), Pbind(\degree, Pseq(~foo[1].flat, inf), \dur,Pseq((~foo[1]+1).flat*0.2, inf))], 1).play;
a.stop;

// instance 2
~cl_cycle.value("~foo2", "INTERLACE-CYCLE", [Array.rand(3, 0, 11).postln, Array.rand(4, 0, 11).postln, Array.rand(5, 0, 11).postln]);
b = Pbind(\degree, Pseq(~foo2.collect({|it| it.asSet.asArray}), inf), \dur,Pseq(~foo2.collect({|it| it.sum*0.04}), inf)).play;
b.stop
*/
