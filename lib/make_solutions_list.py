import glob
import re

def make_line(path_name):
    path_name = path_name[-len("xxxx_xx.ml"):]
    year = path_name[:4]
    day = path_name[5:7]
    return f"({day}, {year}, (module Solutions.Problem_{year}_{day} : Solution.T)); "
    
def main():
    with open("solutions_list.ml", "w") as f:
        f.write("let all = [") # ]
        for path_name in glob.glob("solutions/problem_*.ml"):
            if not re.fullmatch(r"^solutions/problem_\d{4}_\d{2}.ml$", path_name):
                continue
            f.write(make_line(path_name))
        f.write("]")

if __name__ == "__main__":
    main()
        

