import os

def create_meson_test_commands(root_dir):
    for folder, subfolders, files in os.walk(root_dir):
        for file in files:
            if file.endswith('.c3t'):
                test_name = f"Test Suite - {os.path.basename(folder)} - {file.replace('.c3t', '')}"
                test_path = os.path.join('../test/test_suite/', os.path.relpath(folder, root_dir), file)
                print(f"test('{test_name}', py3, args: [test_args, '{test_path}'])")

# Usage
create_meson_test_commands('../test_suite/')
