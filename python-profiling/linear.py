'''
https://www.hackerrank.com/challenges/climbing-the-leaderboard

# Climbing the leaderboard

The function that solves the problem is:

```python
get_positions_per_score(ranks, scores)
```

The complexity of this solution is:
- Time: O(ranks) + O(scores) = O(max(ranks,scores))
- Space: O(ranks) + O(scores) = O(max(ranks,scores))
'''

from collections import deque

import io_functions as io
import cProfile
import pstats


def remove_duplicates(ranks: list[int]) -> list[int]:
    '''
    Given a list sorted in descending order (ranks),
    remove the duplicates of the list.

    Time complexity: O(n)
    Why? See: https://stackoverflow.com/a/7961390/2420718
    '''
    return list(dict.fromkeys(ranks))


def get_positions_per_score(ranks: list[int], scores: list[int]) -> deque[int]:
    '''
    Return the position of each score in the ranks list
    using a zero-based index.

    ranks: list sorted in **descending** order
    scores: list sorted in **descending** order

    Time complexity: O(scores) + O(ranks)
    '''
    positions = deque()  # why a deque? to make all appends O(1)
    ranks_index = 0  # O(1)
    scores_index = 0  # O(1)

    scores_size = len(scores)  # O(1)
    ranks_size = len(ranks)  # O(1)

    # O(scores) + O(ranks)
    while (scores_index < scores_size) and (ranks_index < ranks_size):
        score = scores[scores_index]  # O(1)
        rank = ranks[ranks_index]  # O(1)
        if score >= rank:  # O(1)
            positions.append(ranks_index)  # O(1)
            scores_index += 1  # O(1)
        else:
            ranks_index += 1  # O(1)

    # add missing scores
    while scores_index < scores_size:  # O(scores) in the worst case
        positions.append(ranks_index)  # O(1)
        scores_index += 1  # O(1)

    positions.reverse()  # O(scores)
    return positions


def solve_problem(ranks: list[int], scores: list[int]) -> list[int]:
    ranks = remove_duplicates(ranks)  # O(ranks)
    scores.reverse()  # O(scores)
    positions = get_positions_per_score(ranks, scores)  # O(ranks) + O(scores)
    one_index_positions = list(map(lambda position: position + 1, positions))
    return one_index_positions


if __name__ == '__main__':
    case = '08'
    ranks_input, scores_input = io.read_case_from_file(f'input{case}.txt')

    profiler = cProfile.Profile()
    profiler.enable()
    positions = solve_problem(ranks_input, scores_input)
    profiler.disable()

    expected_results = io.read_results_from_file(f'output{case}.txt')

    assert len(expected_results) == len(positions)

    for index, _ in enumerate(positions):
        expected = expected_results[index]
        real = positions[index]
        assert expected == real, f'{expected} != {real}, index: {index}'

    # Print the stats report
    stats = pstats.Stats(profiler)
    stats.strip_dirs()
    stats.sort_stats('cumtime')
    stats.print_stats()
