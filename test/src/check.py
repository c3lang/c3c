import re
import sys
import subprocess

def run_tester_script(script_path, test_args):
    try:
        # Run the tester.py script with the provided arguments
        completed_process = subprocess.run(['python3', script_path] + test_args, capture_output=True, text=True, check=True)

        # Get the standard output from the script
        output = completed_process.stdout

        # Return the output for further processing
        return output
    except subprocess.CalledProcessError as e:
        print("Error running tester.py:", e)
        return None

def check_test_output(output):
    # Regular expression to find the 'X / Y' pattern
    match = re.search(r'(\d+) / (\d+)', output)

    if match:
        passed_tests = int(match.group(1))
        total_tests = int(match.group(2))

        # Check if the number of passed tests is equal to the total number of tests
        if passed_tests == total_tests:
            return 0  # Success
        else:
            return 1  # Failure
    else:
        return 1  # Failure due to not finding test results

# Main execution
if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: python script.py path_to_tester.py [test_args]")
        sys.exit(1)

    tester_script_path = sys.argv[1]
    tester_args = sys.argv[2:]

    test_output = run_tester_script(tester_script_path, tester_args)

    if test_output is not None:
        result = check_test_output(test_output)
        sys.exit(result)
    else:
        sys.exit(1)
