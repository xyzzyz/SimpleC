module CodeGenerator(AssemblyInstruction(..), Assembly, generateAssembly) where
import IR
import TypeChecker

data AssemblyInstruction = Instr 

type Assembly = [AssemblyInstruction]

emit = (:)

generateAssembly ir env = ()