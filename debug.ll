; ModuleID = 'test'
source_filename = "test"

%str = type { i8*, i8* }

@print_str = external global { i8* }
@"jit::test::test_ifs" = constant { i8* } { i8* bitcast ({ i8* }* ({ i8* }*, i1)* @"jit::test::test_ifs.1" to i8*) }
@"jit::test::show_something_else" = constant { i8* } { i8* bitcast (i1 ({ i8* }*, {})* @"jit::test::show_something_else.3" to i8*) }
@"jit::test::show_short_curicuit" = constant { i8* } { i8* bitcast (void ({ i8* }*, {})* @"jit::test::show_short_curicuit.4" to i8*) }
@"jit::test::test_ifexpr" = constant { i8* } { i8* bitcast ({ i8* }* ({ i8* }*, i1)* @"jit::test::test_ifexpr.5" to i8*) }
@"jit::test::main" = constant { i8* } { i8* bitcast (void ({ i8* }*, {})* @"jit::test::main.7" to i8*) }
@"jit::test::show_something" = constant { i8* } { i8* bitcast (i1 ({ i8* }*, {})* @"jit::test::show_something.8" to i8*) }
@0 = constant [2 x i8] c"a\0A"
@1 = constant [2 x i8] c"b\0A"
@2 = constant [8 x i8] c"neither\0A"
@3 = constant [15 x i8] c"general kenobi\0A"
@"()" = constant {} zeroinitializer
@4 = constant [20 x i8] c"this is a seperator\0A"
@5 = constant [18 x i8] c"another seperator\0A"
@6 = constant [22 x i8] c"yet another seperator\0A"
@7 = constant [3 x i8] c"one"
@8 = constant [3 x i8] c"two"

define { i8* }* @"jit::test::test_ifs.1"({ i8* }* %0, i1 %1) {
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i8*, i1 }* getelementptr ({ i8*, i1 }, { i8*, i1 }* null, i32 1) to i32))
  %ret = bitcast i8* %malloccall to { i8*, i1 }*
  %3 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %ret, i32 0, i32 0
  store i8* bitcast (i32 ({ i8* }*, i1)* @"jit::test::test_ifs.2" to i8*), i8** %3, align 8
  %4 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %ret, i32 0, i32 1
  store i1 %1, i1* %4, align 1
  %5 = bitcast { i8*, i1 }* %ret to { i8* }*
  ret { i8* }* %5
}

define i32 @"jit::test::test_ifs.2"({ i8* }* %0, i1 %1) {
arg_declarations:
  %ret = alloca i32, align 4
  %2 = bitcast { i8* }* %0 to { i8*, i1 }*
  %a = alloca i1, align 1
  %3 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %2, i32 0, i32 1
  %4 = load i1, i1* %3, align 1
  store i1 %4, i1* %a, align 1
  %b = alloca i1, align 1
  store i1 %1, i1* %b, align 1
  br label %start

start:                                            ; preds = %arg_declarations
  %5 = load i1, i1* %a, align 1
  br i1 %5, label %6, label %10

6:                                                ; preds = %start
  %7 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @0, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @0, i64 1, i32 0) }, %str* %7, align 8
  %8 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %9 = bitcast i8* %8 to void ({ i8* }*, %str*)*
  call void %9({ i8* }* @print_str, %str* %7)
  br label %16

10:                                               ; preds = %start
  %11 = load i1, i1* %b, align 1
  br i1 %11, label %17, label %12

12:                                               ; preds = %10
  %13 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([8 x i8], [8 x i8]* @2, i32 0, i32 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* @2, i64 1, i32 0) }, %str* %13, align 8
  %14 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %15 = bitcast i8* %14 to void ({ i8* }*, %str*)*
  call void %15({ i8* }* @print_str, %str* %13)
  br label %16

16:                                               ; preds = %12, %17, %6
  store i32 0, i32* %ret, align 4
  br label %ret1

17:                                               ; preds = %10
  %18 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 1, i32 0) }, %str* %18, align 8
  %19 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %20 = bitcast i8* %19 to void ({ i8* }*, %str*)*
  call void %20({ i8* }* @print_str, %str* %18)
  br label %16

ret1:                                             ; preds = %16
  %21 = load i32, i32* %ret, align 4
  ret i32 %21
}

declare noalias i8* @malloc(i32)

define i1 @"jit::test::show_something_else.3"({ i8* }* %0, {} %1) {
arg_declarations:
  %ret = alloca i1, align 1
  %_ = alloca {}, align 8
  store {} %1, {}* %_, align 1
  br label %start

start:                                            ; preds = %arg_declarations
  %2 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([15 x i8], [15 x i8]* @3, i32 0, i32 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @3, i64 1, i32 0) }, %str* %2, align 8
  %3 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %4 = bitcast i8* %3 to void ({ i8* }*, %str*)*
  call void %4({ i8* }* @print_str, %str* %2)
  store i1 false, i1* %ret, align 1
  br label %ret1

ret1:                                             ; preds = %start
  %5 = load i1, i1* %ret, align 1
  ret i1 %5
}

define void @"jit::test::show_short_curicuit.4"({ i8* }* %0, {} %1) {
arg_declarations:
  %_ = alloca {}, align 8
  store {} %1, {}* %_, align 1
  br label %start

start:                                            ; preds = %arg_declarations
  %2 = alloca i1, align 1
  %3 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something", i32 0, i32 0), align 8
  %4 = bitcast i8* %3 to i1 ({ i8* }*, {}*)*
  %5 = call i1 %4({ i8* }* @"jit::test::show_something", {}* @"()")
  br i1 %5, label %16, label %15

6:                                                ; preds = %20
  %7 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([20 x i8], [20 x i8]* @4, i32 0, i32 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @4, i64 1, i32 0) }, %str* %7, align 8
  %8 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %9 = bitcast i8* %8 to void ({ i8* }*, %str*)*
  call void %9({ i8* }* @print_str, %str* %7)
  br label %10

10:                                               ; preds = %6, %20
  %11 = alloca i1, align 1
  %12 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something", i32 0, i32 0), align 8
  %13 = bitcast i8* %12 to i1 ({ i8* }*, {}*)*
  %14 = call i1 %13({ i8* }* @"jit::test::show_something", {}* @"()")
  br i1 %14, label %31, label %32

15:                                               ; preds = %start
  store i1 false, i1* %2, align 1
  br label %20

16:                                               ; preds = %start
  %17 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something_else", i32 0, i32 0), align 8
  %18 = bitcast i8* %17 to i1 ({ i8* }*, {}*)*
  %19 = call i1 %18({ i8* }* @"jit::test::show_something_else", {}* @"()")
  store i1 %19, i1* %2, align 1
  br label %20

20:                                               ; preds = %16, %15
  %21 = load i1, i1* %2, align 1
  br i1 %21, label %6, label %10

22:                                               ; preds = %36
  %23 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([18 x i8], [18 x i8]* @5, i32 0, i32 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @5, i64 1, i32 0) }, %str* %23, align 8
  %24 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %25 = bitcast i8* %24 to void ({ i8* }*, %str*)*
  call void %25({ i8* }* @print_str, %str* %23)
  br label %26

26:                                               ; preds = %22, %36
  %27 = alloca i1, align 1
  %28 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something_else", i32 0, i32 0), align 8
  %29 = bitcast i8* %28 to i1 ({ i8* }*, {}*)*
  %30 = call i1 %29({ i8* }* @"jit::test::show_something_else", {}* @"()")
  br i1 %30, label %43, label %44

31:                                               ; preds = %10
  store i1 true, i1* %11, align 1
  br label %36

32:                                               ; preds = %10
  %33 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something_else", i32 0, i32 0), align 8
  %34 = bitcast i8* %33 to i1 ({ i8* }*, {}*)*
  %35 = call i1 %34({ i8* }* @"jit::test::show_something_else", {}* @"()")
  store i1 %35, i1* %11, align 1
  br label %36

36:                                               ; preds = %32, %31
  %37 = load i1, i1* %11, align 1
  br i1 %37, label %22, label %26

38:                                               ; preds = %48
  %39 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([22 x i8], [22 x i8]* @6, i32 0, i32 0), i8* getelementptr inbounds ([22 x i8], [22 x i8]* @6, i64 1, i32 0) }, %str* %39, align 8
  %40 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %41 = bitcast i8* %40 to void ({ i8* }*, %str*)*
  call void %41({ i8* }* @print_str, %str* %39)
  br label %42

42:                                               ; preds = %38, %48
  br label %ret

43:                                               ; preds = %26
  store i1 true, i1* %27, align 1
  br label %48

44:                                               ; preds = %26
  %45 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @"jit::test::show_something", i32 0, i32 0), align 8
  %46 = bitcast i8* %45 to i1 ({ i8* }*, {}*)*
  %47 = call i1 %46({ i8* }* @"jit::test::show_something", {}* @"()")
  store i1 %47, i1* %27, align 1
  br label %48

48:                                               ; preds = %44, %43
  %49 = load i1, i1* %27, align 1
  br i1 %49, label %38, label %42

ret:                                              ; preds = %42
  ret void
}

define { i8* }* @"jit::test::test_ifexpr.5"({ i8* }* %0, i1 %1) {
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i8*, i1 }* getelementptr ({ i8*, i1 }, { i8*, i1 }* null, i32 1) to i32))
  %ret = bitcast i8* %malloccall to { i8*, i1 }*
  %3 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %ret, i32 0, i32 0
  store i8* bitcast (i32 ({ i8* }*, i1)* @"jit::test::test_ifexpr.6" to i8*), i8** %3, align 8
  %4 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %ret, i32 0, i32 1
  store i1 %1, i1* %4, align 1
  %5 = bitcast { i8*, i1 }* %ret to { i8* }*
  ret { i8* }* %5
}

define i32 @"jit::test::test_ifexpr.6"({ i8* }* %0, i1 %1) {
arg_declarations:
  %ret = alloca i32, align 4
  %2 = bitcast { i8* }* %0 to { i8*, i1 }*
  %a = alloca i1, align 1
  %3 = getelementptr inbounds { i8*, i1 }, { i8*, i1 }* %2, i32 0, i32 1
  %4 = load i1, i1* %3, align 1
  store i1 %4, i1* %a, align 1
  %b = alloca i1, align 1
  store i1 %1, i1* %b, align 1
  br label %start

start:                                            ; preds = %arg_declarations
  %5 = load i1, i1* %a, align 1
  br i1 %5, label %6, label %7

6:                                                ; preds = %start
  br label %19

7:                                                ; preds = %start
  %8 = load i1, i1* %b, align 1
  br i1 %8, label %9, label %10

9:                                                ; preds = %7
  br label %19

10:                                               ; preds = %7
  br i1 true, label %11, label %13

11:                                               ; preds = %10
  %x = alloca i32, align 4
  store i32 2, i32* %x, align 4
  %12 = load i32, i32* %x, align 4
  br label %19

13:                                               ; preds = %10
  %14 = load i1, i1* %a, align 1
  %15 = load i1, i1* %b, align 1
  %16 = icmp eq i1 %14, %15
  br i1 %16, label %17, label %18

17:                                               ; preds = %13
  br label %19

18:                                               ; preds = %13
  br label %19

19:                                               ; preds = %18, %17, %11, %9, %6
  %20 = phi i32 [ 0, %6 ], [ 1, %9 ], [ %12, %11 ], [ 3, %17 ], [ 4, %18 ]
  store i32 %20, i32* %ret, align 4
  br label %ret1

ret1:                                             ; preds = %19
  %21 = load i32, i32* %ret, align 4
  ret i32 %21
}

define void @"jit::test::main.7"({ i8* }* %0, {} %1) {
arg_declarations:
  %_ = alloca {}, align 8
  store {} %1, {}* %_, align 1
  br label %start

ret:                                              ; No predecessors!
  ret void

start:                                            ; preds = %arg_declarations
  %a = alloca i1, align 1
  store i1 true, i1* %a, align 1
  %x = alloca i32, align 4
  store i32 0, i32* %x, align 4
  %2 = load i32, i32* %x, align 4
  switch i32 %2, label %11 [
    i32 1, label %3
    i32 2, label %7
  ]

3:                                                ; preds = %start
  %4 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([3 x i8], [3 x i8]* @7, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @7, i64 1, i32 0) }, %str* %4, align 8
  %5 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %6 = bitcast i8* %5 to void ({ i8* }*, %str*)*
  call void %6({ i8* }* @print_str, %str* %4)
  br label %11

7:                                                ; preds = %start
  %8 = alloca %str, align 8
  store %str { i8* getelementptr inbounds ([3 x i8], [3 x i8]* @8, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8], [3 x i8]* @8, i64 1, i32 0) }, %str* %8, align 8
  %9 = load i8*, i8** getelementptr inbounds ({ i8* }, { i8* }* @print_str, i32 0, i32 0), align 8
  %10 = bitcast i8* %9 to void ({ i8* }*, %str*)*
  call void %10({ i8* }* @print_str, %str* %8)
  br label %11

11:                                               ; preds = %start, %7, %3
}

declare i1 @"jit::test::show_something.8"({ i8* }*, {})
