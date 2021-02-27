 def identity(arg) = arg

def soMuchFun0(arg) = def foo(a) = a
def soMuchFun(arg) = identity ( arg )

def evenMoreFun(arg) = soMuchFun ( soMuchFun ( arg))

println ( addOne(addOne(addOne(addOne ( 42)))) )

println(currentTime)
