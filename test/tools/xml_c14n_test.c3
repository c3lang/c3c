/**
 * xml_c14n_test — Validate C3's XML parser+encoder via canonical-form comparison.
 *
 * For each .xml file: run xmllint --c14n on the original (baseline), then parse
 * and re-encode with C3, run xmllint --c14n on that output, and compare.
 * If xmllint rejects the original, verify C3 also rejects it.
 *
 * Build:  ./build/c3c compile -o /tmp/xml_c14n_test test/tools/xml_c14n_test.c3
 * Usage:  /tmp/xml_c14n_test file.xml [file2.xml ...] | /path/to/dir/
 */
module xml_c14n_test;
import std, std::encoding::xml, std::os, std::os::process;
import std::io::path;

int g_pass, g_fail, g_skip, g_err;

fn String? capture_stdout(Allocator alloc, String[] cmd) @local
{
    Process proc = process::spawn(cmd, INHERIT_ENV)!;
    defer proc.destroy();

    // Drain stdout while child runs (avoids pipe-buffer deadlock).
    DString buf = dstring::new(alloc);
    char[8192] tmp;
    while (true)
    {
        usz? nr = proc.read_stdout(&tmp[0], tmp.len);
        if (catch err = nr) break;
        if (nr == 0) break;
        buf.append((String)tmp[:nr]);
    }

    CInt ec = proc.join()!;
    if (ec != 0) return process::FAILED_TO_START_PROCESS~;
    return buf.str_view();
}

fn String temp_path(Allocator alloc) @local
    => string::format(alloc, "/tmp/xml_c14n_%d.xml", posix::getpid());

fn void test_file(String path) @local
{
    @pool()
    {
        String[] cmd_orig = { "xmllint", "--nowarning", "--c14n", path };
        String? baseline = capture_stdout(tmem, cmd_orig);
        String base_dir = "";
        foreach_r (i, ch : path)
        {
            if (ch == '/') { base_dir = path[:i + 1]; break; }
        }
        XmlFileEntityResolver resolver = { .base_dir = base_dir };
        XmlParseOptions parse_opts = {
            .keep_comments                = true,
            .keep_processing_instructions = true,
            .entity_resolver              = &xml::resolve_file_entity,
            .entity_resolver_data         = &resolver,
        };

        if (catch err = baseline)
        {
            char[]? raw = file::load(tmem, path);
            if (catch err2 = raw) { g_skip++; return; }
            XmlDoc? doc = xml::parse_string(tmem, (String)raw, parse_opts);
            if (catch err2 = doc)
            {
                g_pass++;
                io::printfn("PASS  %s  (correctly rejected)", path);
            }
            else
            {
                g_fail++;
                io::printfn("FAIL  %s  (accepted invalid XML that xmllint rejected)", path);
            }
            return;
        }

        char[]? raw = file::load(tmem, path);
        if (catch err = raw)
        {
            g_err++;
            io::printfn("ERR   %s  (read: %s)", path, err);
            return;
        }
        XmlDoc? doc = xml::parse_string(tmem, (String)raw, parse_opts);
        if (catch err = doc)
        {
            g_err++;
            io::printfn("PARSE_ERR  %s  (%s)", path, err);
            return;
        }
        String encoded = doc.tencode();

        String tmp = temp_path(tmem);
        if (catch err = file::save(tmp, (char[])encoded))
        {
            g_err++;
            io::printfn("ERR   %s  (cannot write temp file)", path);
            return;
        }
        defer (void)file::delete(tmp);

        String[] cmd_c3 = { "xmllint", "--nowarning", "--c14n", tmp };
        String? c3_c14n = capture_stdout(tmem, cmd_c3);
        if (catch err = c3_c14n)
        {
            g_err++;
            io::printfn("ENCODE_ERR  %s  (xmllint --c14n failed on C3 output)", path);
            return;
        }

        if (baseline == c3_c14n)
        {
            g_pass++;
            io::printfn("PASS  %s", path);
        }
        else
        {
            g_fail++;
            io::printfn("FAIL  %s", path);
            print_first_diff(baseline, c3_c14n);
        }
    };
}

fn void print_first_diff(String a, String b) @local
{
    String[] al = a.tsplit("\n");
    String[] bl = b.tsplit("\n");
    usz n = al.len < bl.len ? al.len : bl.len;
    for (usz i = 0; i < n; i++)
    {
        if (al[i] != bl[i])
        {
            io::printfn("    first diff at line %d:", i + 1);
            io::printfn("    < %s", al[i]);
            io::printfn("    > %s", bl[i]);
            return;
        }
    }
    if (al.len != bl.len)
    {
        io::printfn("    line count differs: baseline=%d  c3=%d", al.len, bl.len);
    }
}

fn bool? visit(Path p, bool is_dir, any data) @local
{
    if (is_dir) return false;
    String s = p.str_view();
    if (s.ends_with(".xml")) test_file(s);
    return false;
}

fn int main(String[] args)
{
    if (args.len < 2)
    {
        io::eprintfn("Usage: %s <file.xml|dir> [...]", args[0]);
        return 1;
    }

    foreach (arg : args[1..])
    {
        if (file::is_file(arg))
        {
            test_file(arg);
        }
        else if (file::is_dir(arg))
        {
            Path? p = path::temp(arg);
            if (catch err = p)
            {
                io::eprintfn("Warning: cannot open directory '%s'", arg);
                continue;
            }
            path::traverse(p, &visit, null)!!;
        }
        else
        {
            io::eprintfn("Warning: '%s' is not a file or directory", arg);
        }
    }

    io::printfn("\nResults: %d passed, %d failed, %d errors, %d skipped",
        g_pass, g_fail, g_err, g_skip);
    return (g_fail > 0 || g_err > 0) ? 1 : 0;
}
