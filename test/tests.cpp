#include <string>
#include <array>
#include <iostream>
#include <sstream>
#include <functional>
#include <type_traits>
#include <numeric>
#include <vector>

#define PG_LEX_TESTS 1

#include "lex.h"

#ifdef __has_cpp_attribute
# if __has_cpp_attribute(maybe_unused)
#  define PG_LEX_MAYBE_UNUSED [[maybe_unused]]
# endif
#endif
#ifndef PG_LEX_MAYBE_UNUSED
# define PG_LEX_MAYBE_UNUSED
#endif

using namespace pg;
using namespace std::literals;

static int total_checks  = 0;
static int failed_checks = 0;

static bool report_failed_check(const char * const expected, const char * const file, const int line, const char * const condition)
{
    std::cout << expected << ": (file " << file << ", line " << line << "): " << condition << '\n';
    ++failed_checks;
    return false;
}

static bool report_exception(const char * const exception, const char* const file, const int line, const char * const condition)
{
    std::cout << "Exception: '" << exception << "' (file " << file << ", line " << line << "): " << condition << '\n';
    ++failed_checks;
    return false;
}

#define assert_true(c) do { ++total_checks; try { (c) || report_failed_check("Expected 'true'", __FILE__, __LINE__, #c); } catch(const pg::lex::lex_error & e){ report_exception(e.what(), __FILE__, __LINE__, #c); } } while(false)
#define assert_false(c) do { ++total_checks; try { !(c) || report_failed_check("Expected 'false'", __FILE__, __LINE__, #c); } catch(const pg::lex::lex_error & e){ report_exception(e.what(), __FILE__, __LINE__, #c); } } while(false)

static void match()
{
#if PG_LEX_TESTS
    {
        assert_true(lex::match("aaab", ".*b").at(0) == "aaab");
        assert_true(lex::match("aaa", ".*a").at(0) == "aaa");
        assert_true(lex::match("b", ".*b").at(0) == "b");

        assert_true(lex::match("aaab", ".+b").at(0) == "aaab");
        assert_true(lex::match("aaa", ".+a").at(0) == "aaa");
        assert_false(lex::match("b", ".+b"));

        assert_true(lex::match("aaab", ".?b").at(0) == "ab");
        assert_true(lex::match("aaa", ".?a").at(0) == "aa");
        assert_true(lex::match("b", ".?b").at(0) == "b");

        assert_true(lex::match("alo xyzK", "(%w+)K").at(0) == "xyz");
        assert_true(lex::match("254 K", "(%d*)K").at(0) == "");
        assert_true(lex::match("alo ", "(%w*)$").at(0) == "");
        assert_false(lex::match("alo ", "(%w+)$"));

        auto result = lex::match("testtset", "^(tes(t+)set)$");
        assert_true(result);
        assert_true(result.at(0) == "testtset");
        assert_true(result.at(1) == "tt");
    }

    {
        auto result = lex::match("", "");    // empty patterns are tricky
        assert_true(result.position().first == 0);
        assert_true(result.position().second == 0);
    }

    {
        auto result = lex::match("alo", "");
        assert_true(result.position().first == 0);
        assert_true(result.position().second == 0);
    }

    {
        assert_true(lex::match("alo123alo", "12").position().first == 3);
        assert_false(lex::match("alo123alo", "^12"));
    }

    const auto f = [](std::string str, std::string pat)
    {
        auto result = lex::match(str, pat);
        if(result)
        {
            auto result_string = std::string(str.data() + result.position().first, str.data() + result.position().second);
            return result_string;
        }
        return std::string();
    };

    {
        assert_true(f("aloALO", "%l*") == "alo");
        assert_true(f("aLo_ALO", "%a*") == "aLo");
        assert_true(f("  \n\r*&\n\r   xuxu  \n\n", "%g%g%g+") == "xuxu");
        assert_true(f("aaab", "a*") == "aaa");
        assert_true(f("aaa", "^.*$") == "aaa");
        assert_true(f("aaa", "b*") == "");
        assert_true(f("aaa", "ab*a") == "aa");
        assert_true(f("aba", "ab*a") == "aba");
        assert_true(f("aaab", "a+") == "aaa");
        assert_true(f("aaa", "^.+$") == "aaa");
        assert_false(lex::match("aaa", "b+"));
        assert_false(lex::match("aaa", "ab+a"));
        assert_true(f("aba", "ab+a") == "aba");
        assert_true(f("a$a", ".$") == "a");
        assert_true(f("a$a", ".%$") == "a$");
        assert_true(f("a$a", ".$.") == "a$a");
        assert_false(lex::match("a$a", "$$"));
        assert_false(lex::match("a$b", "a$"));
        assert_true(f("a$a", "$") == "");
        assert_true(f("", "b*") == "");
        assert_false(lex::match("aaa", "bb*"));
        assert_true(f("aaab", "a-") == "");
        assert_true(f("aaa", "^.-$") == "aaa");
        assert_true(f("aabaaabaaabaaaba", "b.*b") == "baaabaaabaaab");
        assert_true(f("aabaaabaaabaaaba", "b.-b") == "baaab");
        assert_true(f("alo xo", ".o$") == "xo");
        assert_true(f(" \n isto é assim", "%S%S*") == "isto");
        assert_true(f(" \n isto é assim", "%S*$") == "assim");
        assert_true(f(" \n isto é assim", "[a-z]*$") == "assim");
        assert_true(f("um caracter ? extra", "[^%sa-z]") == "?");
        assert_true(f("", "a?") == "");
        assert_true(f("á", "á?") == "á");
        assert_true(f("ábl", "á?b?l?") == "ábl");
        assert_true(f("aa", "^aa?a?a") == "aa");
        assert_true(f("0alo alo", "%x*") == "0a");
        assert_true(f("alo alo", "%C+") == "alo alo");
        assert_true(lex::match("(álo)", "%(á").position().first == 0);
        assert_false(lex::match("==", "^([=]*)=%1$"));
        assert_true(lex::match("===", "^([=]*)=%1$"));
        assert_false(lex::match("====", "^([=]*)=%1$"));
        assert_false(lex::match("==========", "^([=]*)=%1$"));
    }

    {
        auto result = lex::match("clo alo", "^(((.).).* (%w*))$");
        assert_true(result.size() == 4);
        assert_true(result.at(0) == "clo alo");
        assert_true(result.at(1) == "cl");
        assert_true(result.at(2) == "c");
        assert_true(result.at(3) == "alo");
    }

    {
        auto str = "0123456789";
        auto result = lex::match(str, "(.+(.?)())");
        assert_true(result.size() == 3);
        assert_true(result.at(0) == "0123456789");
        assert_true(result.at(1) == "");
        assert_true(result.at(2).data() - str == 10);
    }

    {
        assert_true(lex::match("a", "%f[a]").position().first == 0);
        assert_true(lex::match("a", "%f[^%z]").position().first == 0);
        assert_true(lex::match("a", "%f[^%l]").position().first == 1);
        assert_true(lex::match("aba", "%f[a%z]").position().first == 2);
        assert_true(lex::match("aba", "%f[%z]").position().first == 3);
        assert_false(lex::match("aba", "%f[%l%z]"));
        assert_false(lex::match("aba", "%f[^%l%z]"));

        auto result1 = lex::match(" alo aalo allo", "%f[%S].-%f[%s].-%f[%S]");
        assert_true(result1.position().first == 1);
        assert_true(result1.position().second == 5);

        auto result2 = lex::match(" alo aalo allo", "%f[%S](.-%f[%s].-%f[%S])");
        assert_true(result2.at(0) == "alo ");
    }
#endif
}

static void match_pattern()
{
#if PG_LEX_TESTS
    assert_true(lex::match_pat("aaa", lex::pattern<char>("a")));
    assert_true(lex::match_pat("aaa", lex::pattern<char>(std::string("a"))));
    assert_true(lex::match_pat("aaa", lex::pattern<char>("a", 1)));
    assert_true(lex::match_pat("aaa", lex::pattern<char>(static_cast< const char * >("a"))));
#endif
}

static void gmatch()
{
#if PG_LEX_TESTS
    {
        assert_true(*begin(lex::gmatch_context<char, char>("aashing ", lex::pattern<char>("[a-z]shing"))));
    }

    {
        int i = 0;
        for(const auto& mr : lex::gmatch("abcde", "()"))
        {
            assert_true(mr.size() == 1);
            ++i;
        }
        assert_true(i == 6);
    }

    {
        std::vector< std::string > v;
        for(const auto& mr : lex::gmatch("first second word", "%w+"))
        {
            assert_true(mr.size() == 1);
            v.emplace_back(mr.at(0));
        }
        assert_true(v.size() == 3);
        assert_true(v[0] == "first");
        assert_true(v[1] == "second");
        assert_true(v[2] == "word");
    }
    {
        std::vector< int > v = { 2, 5, 8 };
        std::string str      = "xuxx uu ppar r";
        for(const auto& mr : lex::gmatch(str, "()(.)%2"))
        {
            assert_true(mr.size() == 2);
            auto m = mr.at(0);

            assert_true(m.size() == 0);
            assert_true((m.data() - str.data()) == v.front());
            v.erase(v.begin());
        }
        assert_true(v.size() == 0);
    }
    {
        int i = 0;
        for(const auto& mr : lex::gmatch("13 14 10 = 11, 15= 16, 22=23", "(%d+)%s*=%s*(%d+)"))
        {
            assert_true(mr.size() == 2);
            auto m0 = mr.at(0);
            auto m1 = mr.at(1);

            auto l = std::stoi(m0.data());
            auto r = std::stoi(m1.data());
            assert_true((l + 1) == r);
            ++i;
        }
        assert_true(i == 3);
    }

    {
        std::string result;
        std::string str("a  \nbc\t\td");
        int i = 0;
        for(const auto& mr : lex::gmatch(str, "()%s*()"))
        {
            const auto pos     = mr.position();
            const auto sub_str = str.substr(i, pos.first - i);
            i                  = pos.second;
            result.append(sub_str);
            result.append("-");
        }
        assert_true(result == "-a-b-c-d-");
    }

    {
        using iterator = lex::gmatch_iterator< char, char >;

        auto str  = "abcde";

        auto c_1        = lex::gmatch_context<char, char>(str, lex::pattern<char>("ab"));
        auto it_1_begin = iterator(c_1, c_1.s.begin);
        ++it_1_begin;
        assert_true(*it_1_begin);
        assert_true(it_1_begin->position().first == 0);
        auto it_1_end   = lex::end(c_1);
        assert_true(it_1_begin != it_1_end);

        auto c_2        = lex::gmatch_context<char, char>(str, lex::pattern<char>("^ab"));
        auto it_2_begin = iterator(c_2, c_2.s.begin);
        ++it_2_begin;
        assert_true(*it_2_begin);
        assert_true(it_2_begin->position().first == 0);
        auto it_2_end   = lex::end(c_2);
        assert_true(it_2_begin != it_2_end);

        auto c_3        = lex::gmatch_context<char, char>(str, lex::pattern<char>("bc"));
        auto it_3_begin = iterator(c_3, c_3.s.begin);
        ++it_3_begin;
        auto it_3_end   = lex::end(c_3);
        assert_true(it_3_begin != it_3_end);

        auto c_4        = lex::gmatch_context<char, char>(str, lex::pattern<char>("^bc"));
        auto it_4_begin = iterator(c_4, c_4.s.begin);
        ++it_4_begin;
        auto it_4_end   = lex::end(c_4);
        assert_true(it_4_begin != it_4_end);
    }
#endif
}

static void gsub()
{
#if PG_LEX_TESTS
    {
        auto result = lex::gsub("hello world", "(%w+)", "%1 %1");
        assert_true(result == "hello hello world world");
    }

    {
        auto result = lex::gsub("hello world", "(%w+)", "%1 %1", 1);
        assert_true(result == "hello hello world");
    }

    assert_true(lex::gsub("abc", "%w", "%1%0") == "aabbcc");
    assert_true(lex::gsub("abc", "%w+", "%0%1") == "abcabc");
    assert_true(lex::gsub("", "^", "r") == "r");
    assert_true(lex::gsub("", "$", "r") == "r");
    assert_true(lex::gsub("alo alo", "()[al]", "%1") == "12o 56o");
    assert_true(lex::gsub("abc=xyz", "(%w*)(%p)(%w+)", "%3%2%1-%0") == "xyz=abc-abc=xyz");
    assert_true(lex::gsub("a b cd", " *", "-") == "-a-b-c-d-");
    assert_true("@" + lex::gsub("abç d", "(.)", "%1@") == lex::gsub("abç d", "", "@"));
    assert_true(lex::gsub("abçd", "(.)", "%0@", 2) == "a@b@çd");

    {
        auto result1 = lex::gsub_func("hello world", "world", [](const lex::match_result&) { return "pg1003"; });
        assert_true(result1 == "hello pg1003");
        auto result3 = lex::gsub_func("hello world", "world", [](const lex::match_result&) { return std::string("pg1003"); });
        assert_true(result3 == "hello pg1003");
    }

    {
        auto is_balanced = [](const std::string & s)
        {
            return !lex::match(lex::gsub(s, "%b()", ""), "[()]");
        };

        assert_true(lex::gsub("alo 'oi' alo", "%b''", "\"") == "alo \" alo");
    }

    {
        assert_true(lex::gsub("aaa aa a aaa a", "%f[%w]a", "x") == "xaa xa x xaa x");
        assert_true(lex::gsub("[[]] [][] [[[[", "%f[[].", "x") == "x[]] x]x] x[[[");
        assert_true(lex::gsub("01abc45de3", "%f[%d]", ".") == ".01abc.45de.3");
        assert_true(lex::gsub("01abc45 de3x", "%f[%D]%w", ".") == "01.bc45 de3.");
        assert_true(lex::gsub("function", "%f[\x01-\xFF]%w", ".") == ".unction");
        assert_true(lex::gsub("function", "%f[^\x01-\xFF]", ".") == "function.");

        std::vector< int > a = { 0, 4, 8, 13, 16 };
        for(auto& mr : lex::gmatch_context<char, char>("alo alo th02 is 1hat", lex::pattern<char>("()%f[%w%d]")))
        {
            assert_true(a.front() == mr.position().first);
            a.erase(a.cbegin());
        }
        assert_true(a.size() == 0);
    }

    {
        std::array< char, 256 > abc;
        std::iota(abc.begin(), abc.end(), '\0');
        const std::string sv(abc.data(), 256);

        const auto strset = [&](auto p)
        {
            std::string res;
            for(auto mr : lex::gmatch_context<char, char>(sv, lex::pattern<char>(p)))
            {
                res.append(mr.at(0));
            }
            return res;
        };

        assert_true(strset("[\xC8-\xD2]").size() == 11);
        assert_true(strset("[a-z]") == "abcdefghijklmnopqrstuvwxyz");
        assert_true(strset("[a-z%d]") == strset("[%da-uu-z]"));
        assert_true(strset("[a-]") == "-a");
        assert_true(strset("[^%W]") == strset("[%w]"));
        assert_true(strset("[]%%]") == "%]");
        assert_true(strset("[a%-z]") == "-az");
        assert_true(strset("[%^%[%-a%]%-b]") == "-[]^ab");
        assert_true(strset("%Z") == strset("[\x01-\xFF]"));
        assert_true(strset(".") == strset("[\x01-\xFF%z]"));
    }

    /*{
        const auto f = [](auto mr)
        {
            return lex::gsub(mr.at(0), ".", mr.at(1));
        };
        auto result = lex::gsub_func("trocar tudo em |teste|b| é |beleza|al|", "|([^|]*)|([^|]*)|", f);
        assert_true(result == "trocar tudo em bbbbb é alalalalalal");
    }*/
#endif
}

static void gsub_pattern()
{
#if PG_LEX_TESTS
    assert_true(lex::gsub_pat("aaa", lex::pattern<char>("a"), "b") == "bbb");
    assert_true(lex::gsub_pat("aaa", lex::pattern<char>(std::string("a")), "b") == "bbb");
    assert_true(lex::gsub_pat("aaa", lex::pattern<char>("a", 1), "b") == "bbb");
    assert_true(lex::gsub_pat("aaa", lex::pattern<char>(static_cast< const char * >("a")), "b") == "bbb");

    const auto b = [](const lex::match_result &){ return "b"; };

    assert_true(lex::gsub_pat_func("aaa", lex::pattern<char>("a"), b) == "bbb");
    assert_true(lex::gsub_pat_func("aaa", lex::pattern<char>(std::string("a")), b) == "bbb");
    assert_true(lex::gsub_pat_func("aaa", lex::pattern<char>("a", 1), b) == "bbb");
    assert_true(lex::gsub_pat_func("aaa", lex::pattern<char>(static_cast< const char * >("a")), b) == "bbb");
#endif
}

static void exceptions()
{
#if PG_LEX_TESTS
    const auto malform = [](auto pat, lex::error_type ec) -> bool
    {
        try
        {
            return !lex::match("a", pat);
        }
        catch(const lex::lex_error& e)
        {
            if(e.code() == ec)
            {
                return true;
            }
        }
        return false;
    };

    assert_true(malform("(.", lex::capture_not_finished));
    assert_true(malform(".)", lex::capture_invalid_pattern));
    assert_true(malform("[a", lex::pattern_missing_closing_bracket));
    assert_true(malform("[]", lex::pattern_missing_closing_bracket));
    assert_true(malform("[^]", lex::pattern_missing_closing_bracket));
    assert_true(malform("[a%]", lex::pattern_missing_closing_bracket));
    assert_true(malform("[a%", lex::pattern_ends_with_percent));
    assert_true(malform("%b", lex::balanced_no_arguments));
    assert_true(malform("%ba", lex::balanced_no_arguments));
    assert_true(malform("%", lex::pattern_ends_with_percent));
    assert_true(malform("%f", lex::frontier_no_open_bracket));

    const auto checkerror = [](auto str, auto pat, auto repl, lex::error_type ec) -> bool
    {
        try
        {
			PG_LEX_MAYBE_UNUSED auto result = lex::gsub(str, pat, repl);
        }
        catch(const lex::lex_error& e)
        {
            if(e.code() == ec)
            {
                return true;
            }
        }
        return false;
    };
    assert_true(checkerror("alo", ".", "%2", lex::capture_invalid_index));
    assert_true(checkerror("alo", "(%0)", "a", lex::capture_invalid_index));
    assert_true(checkerror("alo", "(%1)", "a", lex::capture_invalid_index));
    assert_true(checkerror("alo", ".", "%x", lex::percent_invalid_use_in_replacement));

    try
    {
        auto mr = lex::match("foo", "...");
		PG_LEX_MAYBE_UNUSED auto match =  mr.at(1);
        assert_true(0);
    }
    catch(const lex::lex_error& e)
    {
        assert_true(e.code() == lex::capture_out_of_range);
    }
#endif
}

static void results()
{
#if PG_LEX_TESTS
    {
        int i = 0;
        for(auto& mr : lex::gmatch_context<char, char>("13 14 10 = 11, 15= 16, 22=23", lex::pattern<char>("(%d+)%s*=%s*(%d+)")))
        {
            assert_true(mr.size() == 2);
            lex::match_result::iterator     it;
            std::vector< std::string > v;

            it = std::cbegin(mr);
            for(; it != std::cend(mr) ; it++)
            {
                v.push_back(*it);
            }
            for(const std::string &sv : mr)
            {
                v.push_back(sv);
            }
            auto l = std::stoi(v[0].data());
            auto r = std::stoi(v[1].data());
            assert_true((l + 1) == r);
            ++i;
        }
        assert_true(i == 3);
    }
#endif
}

static void string_types()
{
#if PG_LEX_TESTS
    const char * const     str   = "aaab";
    const char * const     pat   = ".*b";
    const wchar_t * const  wstr  = L"aaab";
    const wchar_t * const  wpat  = L".*b";
#if defined(__cpp_lib_char8_t)
    const char8_t * const str8 = u8"aaab";
    const char8_t * const pat8 = u8".*b";
#endif
    const char16_t * const str16 = u"aaab";
    const char16_t * const pat16 = u".*b";
    const char32_t * const str32 = U"aaab";
    const char32_t * const pat32 = U".*b";

    std::string    string("aaab");
    std::string    pattern(".*b");
    std::wstring   wstring(L"aaab");
    std::wstring   wpattern(L".*b");
#if defined(__cpp_lib_char8_t)
    std::u8string  string8(u8"aaab");
    std::u8string  pattern8(u8".*b");
#endif
    std::u16string string16(u"aaab");
    std::u16string pattern16(u".*b");
    std::u32string string32(U"aaab");
    std::u32string pattern32(U".*b");

    assert_true(lex::match("aaab", ".*b"));
    assert_true(lex::match(L"aaab", ".*b"));
    assert_true(lex::match(u"aaab", ".*b"));
    assert_true(lex::match(U"aaab", U".*b"));

    assert_true(lex::match(str, pat));
    assert_true(lex::match(wstr, wpat));
#if defined(__cpp_lib_char8_t)
    assert_true(lex::match(str8, pat8));
#endif
    assert_true(lex::match(str16, pat16));
    assert_true(lex::match(str32, pat32));

    assert_true(lex::match(str, pat32));
    assert_true(lex::match(wstr, pat32));
#if defined(__cpp_lib_char8_t)
    assert_true(lex::match(str8, pat32));
#endif
    assert_true(lex::match(str16, pat32));
    assert_true(lex::match(str32, pat));

    assert_true(lex::match(string, pattern));
    assert_true(lex::match(wstring, wpattern));
#if defined(__cpp_lib_char8_t)
    assert_true(lex::match(string8, pattern8));
#endif
    assert_true(lex::match(string16, pattern16));
    assert_true(lex::match(string32, pattern32));

    assert_true(lex::match(string, wpattern));
    assert_true(lex::match(wstring, pattern16));
#if defined(__cpp_lib_char8_t)
    assert_true(lex::match(string8, wpattern));
#endif
    assert_true(lex::match(string16, wpattern));
    assert_true(lex::match(string32, wpattern));

    assert_true(lex::match(std::string("aaab"), ".*b"));
    assert_true(lex::match(std::wstring(L"aaab"), ".*b"));
#if defined(__cpp_lib_char8_t)
    assert_true(lex::match(std::u8string(u8"aaab"), ".*b"));
#endif
    assert_true(lex::match(std::u16string(u"aaab"), ".*b"));
    assert_true(lex::match(std::u32string(U"aaab"), U".*b"));

#endif
}

static void string_traits()
{
#if PG_LEX_TESTS
    assert_true((std::is_same< lex::detail::string_traits< char * >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char * >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< char [] >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char [] >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< char [42] >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char [42] >::char_type, char >::value));

    assert_true((std::is_same< lex::detail::string_traits< wchar_t * >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const wchar_t * >::char_type, wchar_t >::value));;
    assert_true((std::is_same< lex::detail::string_traits< wchar_t [] >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const wchar_t [] >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< wchar_t [42] >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const wchar_t [42] >::char_type, wchar_t >::value));

#if defined(__cpp_lib_char8_t)
    assert_true((std::is_same< lex::detail::string_traits< char8_t * >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char8_t * >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char8_t [] >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char8_t [] >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char8_t [42] >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char8_t [42] >::char_type, char8_t >::value));
#endif

    assert_true((std::is_same< lex::detail::string_traits< char16_t * >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char16_t * >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char16_t [] >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char16_t [] >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char16_t [42] >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char16_t [42] >::char_type, char16_t >::value));

    assert_true((std::is_same< lex::detail::string_traits< char32_t * >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char32_t * >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char32_t [] >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char32_t [] >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< char32_t [42] >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const char32_t [42] >::char_type, char32_t >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::string >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::string & >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::string >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::string & >::char_type, char >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::wstring >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::wstring & >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::wstring >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::wstring & >::char_type, wchar_t >::value));

#if defined(__cpp_lib_char8_t)
    assert_true((std::is_same< lex::detail::string_traits< std::u8string >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::u8string & >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u8string >::char_type, char8_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u8string & >::char_type, char8_t >::value));
#endif

    assert_true((std::is_same< lex::detail::string_traits< std::u16string >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::u16string & >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u16string >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u16string & >::char_type, char16_t >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::u32string >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::u32string & >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u32string >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u32string & >::char_type, char32_t >::value));

#if defined(__cpp_lib_string_view)
    assert_true((std::is_same< lex::detail::string_traits< std::string_view >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::string_view & >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::string_view >::char_type, char >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::string_view & >::char_type, char >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::wstring_view >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::wstring_view & >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::wstring_view >::char_type, wchar_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::wstring_view & >::char_type, wchar_t >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::u16string_view >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::u16string_view & >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u16string_view >::char_type, char16_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u16string_view & >::char_type, char16_t >::value));

    assert_true((std::is_same< lex::detail::string_traits< std::u32string_view >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< std::u32string_view & >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u32string_view >::char_type, char32_t >::value));
    assert_true((std::is_same< lex::detail::string_traits< const std::u32string_view & >::char_type, char32_t >::value));
#endif
#endif
}

static void readme_examples()
{
#if PG_LEX_TESTS
    {
        std::string str = "Hello world!";
        auto result = lex::match(str, "^%a+");
        assert_true(result.at(0) == "Hello");
    }

    {
        std::wstring str = L"Hello PG1003!";
        auto result = lex::match(str, "(%a+)%d+");
        assert_true(result);
        assert_true(result.at(0) == L"PG");
    }

    {
        std::vector< std::pair< std::u16string, std::u16string > > results;

        auto str = u"foo = 42;   bar= 1337; baz = PG =1003 ;";
        for(auto & match : lex::gmatch(str, "(%a+)%s*=%s*(%d+)%s*;"))
        {
            assert_true(match.size() == 2);
            results.emplace_back(match.at(0), match.at(1));
        }
        assert_true(results.size() == 3);
        assert_true(results[0].first  == u"foo");
        assert_true(results[0].second == u"42");
        assert_true(results[1].first  == u"bar");
        assert_true(results[1].second == u"1337");
        assert_true(results[2].first  == u"PG");
        assert_true(results[2].second == u"1003");
    }

    {
        auto str    = "foo =\t42; bar= 1337; pg =1003 ;";
        auto pat    = "(%a+)%s*=%s*(%d+)%s*;";
        auto repl   = "%1=%2;";
        auto result = lex::gsub(str, pat, repl);

        assert_true(result == "foo=42; bar=1337; pg=1003;");
    }

    {
        auto str = "one two three four";
        auto function = [](const lex::match_result & mr)
        {
            if(mr.at(0) == "one")
            {
                return "PG";
            }

            return "1003";
        };

        assert_true(lex::gsub_func(str, "%s*%w+", function, 2) == "PG1003 three four");
    }

    {
        auto a = pg::lex::gsub("hello world", "(%w+)", "%1 %1");
        assert_true(a == "hello hello world world");

        auto b = pg::lex::gsub("hello world", "%w+", "%0 %0", 1);
        auto c = pg::lex::gsub("hello world", "%w+", "%1 %1", 1);
        assert_true(b == "hello hello world");
        assert_true(b == c);

        auto d = pg::lex::gsub("hello world from Lua", "(%w+)%s*(%w+)", "%2 %1");
        assert_true(d == "world hello Lua from");
    }

    {
        std::array< std::pair< int, int >, 3 > expected = { std::pair<int, int>{ 0, 1 }, std::pair<int, int>{ 2, 2 }, std::pair<int, int>{ 3, 3 } };
        std::vector< std::pair< int, int > >   results;
        for(const auto& match : pg::lex::gmatch_context<char, char>("abc", pg::lex::pattern<char>("()a*()")))
        {
            results.push_back(match.position());
        }

        assert_true(std::equal(expected.cbegin(), expected.cend(), results.cbegin(), results.cend()));
    }
#endif
}

int main(int /* argc */, char * /* argv */[])
{
    try
    {
        match();
        match_pattern();
        gmatch();
        gsub();
        gsub_pattern();
        exceptions();
        results();
        string_types();
        string_traits();
        readme_examples();

        std::cout << "Total tests: " << total_checks << ", Tests failed: " << failed_checks << '\n';

        return failed_checks ? 1 : 0;
    }
    catch(const lex::lex_error& e)
    {
        std::cout << e.what() << '\n';
        return 1;
    }
}
