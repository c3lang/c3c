// #target: macos-x64
module test;

fn void! foo()
{}

fault Abc { AAA }

macro int! bar()
{
	return Abc.AAA!;
}

fn int! baz()
{
	return 2;
}

fn int main()
{
	int x;
	int! y;
	(void)x;
	(void)y;
	(void)foo();
	(void)bar();
	(void)(bar() + 1);
	(void)baz();
	(void)(baz() + 1);
	return 1;
}

/* #expect: test.ll
define i32 @main() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %y.f = alloca i64, align 8
  %retparam = alloca i32, align 4
  %retparam9 = alloca i32, align 4
  store i32 0, ptr %x, align 4
  store i64 0, ptr %y.f, align 8
  store i32 0, ptr %y, align 4
  %0 = load i32, ptr %x, align 4
  %optval = load i64, ptr %y.f, align 8
  %not_err = icmp eq i64 %optval, 0
  br i1 %not_err, label %after_check, label %end_block

after_check:                                      ; preds = %entry
  %1 = load i32, ptr %y, align 4
  br label %end_block

end_block:                                        ; preds = %after_check, %entry
  %2 = call i64 @test.foo()
  %not_err1 = icmp eq i64 %2, 0
  br i1 %not_err1, label %after_check2, label %end_block3

after_check2:                                     ; preds = %end_block
  br label %end_block3

end_block3:                                       ; preds = %after_check2, %end_block
  br label %end_block4

end_block4:                                       ; preds = %end_block3
  br label %end_block5

end_block5:                                       ; preds = %end_block4
  %3 = call i64 @test.baz(ptr %retparam)
  %not_err6 = icmp eq i64 %3, 0
  br i1 %not_err6, label %after_check7, label %end_block8

after_check7:                                     ; preds = %end_block5
  %4 = load i32, ptr %retparam, align 4
  br label %end_block8

end_block8:                                       ; preds = %after_check7, %end_block5
  %5 = call i64 @test.baz(ptr %retparam9)
  %not_err10 = icmp eq i64 %5, 0
  br i1 %not_err10, label %after_check11, label %end_block12

after_check11:                                    ; preds = %end_block8
  %6 = load i32, ptr %retparam9, align 4
  %add = add i32 %6, 1
  br label %end_block12

end_block12:                                      ; preds = %after_check11, %end_block8
  ret i32 1
}
