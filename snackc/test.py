#!/bin/pypy3
import glob
import subprocess
import itertools
import argparse
def test_file(f: str):
    snackc_result = subprocess.run(["./target/release/snackc", f], capture_output=True)
    #program_output = snackc_result.stdout.decode()
    #snack_path = f
    snack_rel = f.split("/")[-1]
    bin_path = "./" + f.split(".")[0]
    #bin_rel = bin_path.split("/")[-1]
    text_path = bin_path[2:] + ".txt"
    #text_rel = text_path.split("/")[-1]
    if snackc_result.returncode != 0:
         print(f"{snack_rel}: {snackc_result.stderr.decode()}")
    else:
        try:
            # open the text file and compare the binary output to what is in the text file.
            with open(text_path, "r") as f:
                test_output = f.read()
                bin_result = subprocess.run([bin_path], capture_output=True)
                bin_output = bin_result.stdout.decode()
                if test_output != bin_output:
                    error = (snack_rel, test_output, bin_output)
                    print(f"✘   {snack_rel}")
                    return error, False
                else:
                    print(f"✅  {snack_rel}")
        except Exception:
            print(f"{snack_rel} needs a expected output named {text_path} file to be tested.")
    return None, True


# Initialize parser
parser = argparse.ArgumentParser()

# Adding optional argument
parser.add_argument("-b", "--Break", action='store_true', help = "Break on first failed test")
parser.add_argument("-t", "--Test", help = "Test a single test case")

# Read arguments from command line
args = parser.parse_args()

if args.Break:
    print("Displaying Break as: % s" % args.Break)

if args.Test:
    print("Displaying Test as: % s" % args.Test)



cargo_result = subprocess.run(["cargo", "build", "--release"], capture_output=True)
print(cargo_result.stderr.decode())
print(cargo_result.stdout.decode())

path = args.Test if args.Test else "*.snack"
snack_files = glob.glob(f"snack_tests/{path}")
width = max([len(i) for i in snack_files]) + 10
errors = []
for f in snack_files:
    error, successful = test_file(f)
    if error:
        errors.append(error)
    if not successful and args.Break:
        break

for file_name, expected, got in errors:
    print(f"------{file_name}-------")
    print("[Expected]:")
    print(expected[0:-1])
    print("[Got]:")
    print(got)

if not args.Break:
    # Remove the binary and assembly file from the test directory
    for file in [i for i in glob.glob(f"snack_tests/*")]:
        if not file.endswith(".txt") and not file.endswith(".snack"):
            subprocess.run(["rm", file])

