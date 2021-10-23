def __get_ints(line: str) -> list[int]:
    strings = line.strip().split(' ')
    return list(map(lambda x: int(x), strings))


def read_case_from_file(file_path: str) -> tuple[list[int], list[int]]:
    with open(file_path, 'r') as file:
        lines = file.readlines()
        assert len(lines) == 4

        ranks = __get_ints(lines[1])
        scores = __get_ints(lines[3])
        return ranks, scores

def read_results_from_file(file_path: str) -> list[int]:
    results = []
    with open(file_path, 'r') as file:
        lines = file.readlines()
        for line in lines:
            number = int(line.strip())
            results.append(number)
    
    return results
