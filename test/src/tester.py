#!/usr/bin/python
import os, sys, shutil, subprocess

TEST_DIR = '/tmp/c3test/'
class Config:
    run_skipped = False
    cwd = "."
    numtests = 0

class File:
    def __init__(self, filepath):
        with open(filepath) as reader:
            self.content = reader.read().splitlines()
        self.filename = filepath



class Issues:
    def __init__(self, conf, file, single):
        self.file = file
        self.single = single
        self.line = 0
        self.file_start = 0
        self.line_offset = 0
        self.has_errors = False
        self.error_message = "unknown"
        self.skip = False
        self.cur = 0
        self.current_file = None
        self.errors = {}
        self.warnings = {}
        if single:
            self.current_file = conf.cwd + "/" + file.filename

    def exit_error(self, message):
        print('Error in file ' + self.file.filename + ': ' + message)
        exit(-1)

    def set_failed(self):
        if not self.has_errors: print(" Failed.")
        self.has_errors = True

    def check_line(self, type, file, line, message):
        if file == 'test.c3': file = self.file.filename
        map = {}
        if type == 'Error':
            map = self.errors
        elif type == 'Warning':
            map = self.warnings
        else:
            self.exit_error("Unknown type: " + type)
        key = file + ":" + line
        value = map.get(key)
        if value == None: return False
        if value in message:
            del map[key]
            return True
        else:
            return False

    def parse_result(self, lines):
        for line in lines:
            parts = line.split('|', maxsplit=4)
            if len(parts) != 4: self.exit_error("Illegal error result: " + line);
            if not self.check_line(parts[0], parts[1], parts[2], parts[3]):
                self.set_failed()
                print("Unexpected " + parts[0].lower() + " in " + parts[1] + " line " + parts[2] + ":", end="")
                print('"' + parts[3] + '"')
        if len(self.errors) > 0:
            self.set_failed()
            print("Expected errors that never occurred:")
            num = 1
            for key, value in self.errors.items():
                pos = key.split(":", 2)
                print(str(num) + ". " + pos[0] + " line: " + pos[1] + " expected: \"" + value + "\"")
                num += 1

    def compile(self, args):
        path = os.path.dirname(sys.argv[0]) + "/../../cmake-build-debug/"
        code = subprocess.run(path + 'c3c ' + args, universal_newlines=True, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if code.returncode != 0 and code.returncode != 1:
            self.set_failed()
            print("Error (" + str(code.returncode) + "): " + code.stderr)
            self.has_errors = True
            return
        self.parse_result(code.stderr.splitlines(keepends=False))

    def parse_single(self):
        lines = len(self.file.content)
        while self.line < lines:
            line = self.file.content[self.line].strip()
            if "// #" in line:
                self.parse_trailing_directive(line)
            self.line += 1
        with open(TEST_DIR + 'test.c3', mode='w') as f:
            f.write("\n".join(self.file.content))
            f.write("\n")
        print("- " + self.file.filename + ":", end="")
        self.compile("--test compile " + TEST_DIR + 'test.c3')

        if not self.has_errors:
            print(" Passed.")

    def parse_header_directive(self, line):
        line = line[4:].strip()
        if (line.startswith("warnings:")):
            print("TODO" + line)
            exit(-1)
        elif (line.startswith("file:")):
            line = line[5:].strip()
            print("NEW FILE" + line)
            exit(-1)
        elif (line.startswith("expect:")):
            line = line[7:].strip()
            print("Expect " + line)
            exit(-1)
        else:
            self.exit_error("unknown header directive " + line)

    def parse_trailing_directive(self, line):
        line = line.split('// #', 2)[1].strip()
        if (line.startswith("warning:")):
            print("TODO" + line)
            exit(-1)
        elif (line.startswith("error:")):
            line = line[6:].strip()
            self.errors[self.file.filename + ":%d" % (self.line + 1)] = line
        else:
            self.exit_error("unknown trailing directive " + line)

    def parse_template(self):
        lines = len(self.file.content)
        while self.line < lines:
            line = self.file.content[self.line].strip()
            if line.startswith("// #"):
                self.parse_header_directive(line)
            elif "// #" in line:
                self.parse_trailing_directive(line)
            self.line += 1

        print("parse mult")

    def parse(self):
        if len(self.file.content) == 0: self.exit_error("File was empty")
        is_skip = self.file.content[0].startswith("// #skip")
        if is_skip != self.skip: return

        if is_skip: self.line += 1
        if self.single:
            self.parse_single()
        else:
            self.parse_template()


def usage():
    print("Usage: " + sys.argv[0] +  " <file/dir> [-s]")
    print('')
    print('Options:')
    print("  -s, --skipped       only run skipped tests")
    exit(-1)

def handle_file(filepath, conf):
    if filepath.endswith('.c3'):
        single = True
    elif filepath.endswith('.c3t'):
        single = False
    else:
        return

    shutil.rmtree(TEST_DIR, ignore_errors=True)
    os.mkdir(TEST_DIR, mode = 0o777)

    conf.numtests += 1

    issues = Issues(conf, File(filepath), single)
    issues.parse()



def handle_dir(filepath, conf):
    for file in os.listdir(filepath):
        file = filepath + "/" + file
        if os.path.isdir(file):
            handle_dir(file, conf)
        elif os.path.isfile(file):
            handle_file(file, conf)

def main():
    args = len(sys.argv)
    conf = Config()
    if args != 1 and args > 3: usage()
    if args == 3:
        if (sys.argv[2] != '-s' and sys.argv[2] != '--skipped'): usage()
        conf.run_skipped = True
    filepath = sys.argv[1]
    if filepath.endswith('/'): filepath = filepath[:-1]
    conf.cwd = os.getcwd()
    if os.path.isfile(filepath):
        handle_file(filepath, conf)
    elif os.path.isdir(filepath):
        handle_dir(filepath, conf)
    else:
        usage()

main()
