import glob
import re

def make_line(path_name):
    path_name = path_name[len("solutions/problem_"):]
    year = path_name[:4]
    day = path_name[5:7]
    tag = path_name[7:path_name.find(".ml")]
    return f'({day}, {year}, "{tag}", (module Solutions.Problem_{year}_{day}{tag} : Solution.T)); '
    
def main():
    with open("solutions_list.ml", "w") as f:
        f.write("let all = [") # ]
        for path_name in glob.glob("solutions/problem_*.ml"):
            if not re.fullmatch(r"^solutions/problem_\d{4}_\d{2}[a-z]*\.ml$", path_name):
                continue
            f.write(make_line(path_name))
        f.write("]")

if __name__ == "__main__":
    main()
        

