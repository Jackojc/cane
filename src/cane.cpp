#include <iostream>
#include <string>
#include <chrono>

#include <report.hpp>
#include <view.hpp>
#include <lib.hpp>

int main(int, const char*[]) {
	try {
		std::istreambuf_iterator<char> begin(std::cin), end;
		std::string in(begin, end);

		cane::View src { &*in.begin(), &*in.end() };
		cane::Lexer lx { src };

		if (not cane::utf_validate(src))
			lx.error(cane::Phases::PHASE_ENCODING, src, cane::STR_ENCODING);

		namespace time = std::chrono;

		using clock = time::steady_clock;
		using unit = time::microseconds;

		auto t1 = clock::now();
		cane::Context ctx = cane::compile(lx);
		auto t2 = clock::now();

		time::duration<double, std::micro> t = t2 - t1;
		cane::printlnfmt(CANE_ANSI_FG_YELLOW "took: {}µs" CANE_ANSI_RESET, t.count());

		std::cout.precision(2);
		std::cout << std::fixed;

		for (auto& [dur, note, vel, chan, kind]: ctx.timeline) {
			cane::print(CANE_ANSI_FG_YELLOW "midi", (int)chan, CANE_ANSI_RESET " ");
			// cane::print(CANE_ANSI_FG_BLUE, time / 1000.f, "s" CANE_ANSI_RESET " ");
			cane::print("n:" CANE_ANSI_BOLD, (int)note, CANE_ANSI_RESET " ");
			cane::print("v:" CANE_ANSI_BOLD, (int)vel, CANE_ANSI_RESET " ");
			cane::print("d:" CANE_ANSI_BOLD, dur / 1000.f, "s" CANE_ANSI_RESET " ");
			cane::print(CANE_ANSI_FG_RED, kind, CANE_ANSI_RESET);
			cane::println();
		}
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}
