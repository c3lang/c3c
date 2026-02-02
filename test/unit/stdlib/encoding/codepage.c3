// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
module std::encoding::codepage_test;
import std::encoding::codepage::cp437;

fn void test_cp437() @test
{
	String want = "╔══════════════════════════════════════╗"
                      "║  SYSTEM STATUS: OK - 25°C ± 2°C      ║"
                      "║  Café Menu: Crème Brûlée .... £5.00  ║"
                      "╚══════════════════════════════════════╝";

	char[] bytes = x"C9CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDBB"
		x"BA202053595354454D205354415455533A204F4B202D203235F84320F12032F843202020202020BA"
		x"BA202043616682204D656E753A2043728A6D65204272966C8265202E2E2E2E209C352E30302020BA"
		x"C8CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDBC";

	@pool()
	{
		String got = cp437::decode(tmem, bytes)!!;
		assert(got == want, "cp437 decoding failed: got=%s, want=%s", got, want);
		got = (String)cp437::encode(tmem, got[..])!!;
		assert(got == (String)bytes, "cp437 encoding failed: got=%s, want=%s", got, (String)bytes);

	};
}

