#include <iostream>
#include <string>
#include <chrono>
#include <thread>

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
			lx.error(cane::Phases::ENCODING, src, cane::STR_ENCODING);

		namespace time = std::chrono;

		using clock = time::steady_clock;
		using unit = time::microseconds;

		auto t1 = clock::now();
		cane::Context ctx = cane::compile(lx);
		auto t2 = clock::now();

		time::duration<double, std::micro> t = t2 - t1;
		cane::printlnfmt(CANE_ANSI_FG_YELLOW "took: {}µs" CANE_ANSI_RESET, t.count());

		// for (auto& e: ctx.timeline)
		// 	cane::println(e);


		using namespace std::chrono_literals;

		size_t dt = 0;
		for (auto it = ctx.timeline.begin(); it != ctx.timeline.end();) {
			auto begin = it;

			while (it != ctx.timeline.end() and it->time <= dt) {
				cane::println("SEND ", *it);
				++it;
			}

			std::this_thread::sleep_for(1ms);
			dt++;
		}
	}

	catch (cane::Error) {
		return 1;
	}

	return 0;
}
