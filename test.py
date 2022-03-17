#!/bin/pypy3
import glob
import subprocess
import itertools

snacks = [i for i in glob.glob(f"tests/*.snack")]
width = max([len(i) for i in snacks]) + 10
errors = []
for f in snacks:
    snackc_result = subprocess.run(["snackc", f], capture_output=True)
    program_output = snackc_result.stdout.decode()
    snack_path = f
    snack_rel = f.split("/")[-1]
    bin_path = "./" + f.split(".")[0]
    bin_rel = bin_path.split("/")[-1]
    text_path = bin_path[2:] + ".txt"
    text_rel = text_path.split("/")[-1]
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
                    errors.append((test_output, bin_output))
                    print(f"✘   {snack_rel}")
                    break
                else:
                    print(f"✅  {snack_rel}")
                # Remove the binary and assembly file from the test directory
                subprocess.run(["rm", bin_path[2:], bin_path[2:]+".asm"])
        except Exception as e:
            print(f"{snack_rel} needs a expected output named {text_path} file to be tested.")

for expected, got in errors:
    print(f"------{snack_rel}-------")
    print("[Expected]:")
    print(expected)
    print("[Got]:")
    print(got)
    print("------------------------")
