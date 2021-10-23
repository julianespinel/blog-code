'''
https://www.hackerrank.com/challenges/climbing-the-leaderboard

# Climbing the leaderboard

The function that solves the problem is:

```python
get_positions_per_score(ranks, scores)
```

The complexity of this solution is:
- Time: O(scores) * O(ranks log ranks)
- Space: O(ranks) + O(scores) = O(max(ranks,scores))
'''

import io_functions as io
import cProfile
import pstats


def remove_duplicates(ranks: list[int]) -> list[int]:
    '''
    Time complexity: O(n log n)
    '''
    ranks_set = set(ranks)
    return sorted(ranks_set, reverse=True)


def get_positions_per_score(ranks: list[int], scores: list[int]) -> list[int]:
    '''
    Time complexity: O(scores) * O(ranks log ranks)
    '''
    positions = []
    for score in scores:  # O(scores)
        # amortized O(1), why? see: https://stackoverflow.com/a/33045038/2420718
        ranks.append(score)
        ranks = sorted(ranks, reverse=True)  # O(ranks log ranks)
        position = ranks.index(score)  # O(ranks)
        positions.append(position)  # amortized O(1)

    return positions


def solve_problem(ranks: list[int], scores: list[int]) -> list[int]:
    ranks = remove_duplicates(ranks)  # O(ranks log ranks)
    positions = get_positions_per_score(ranks, scores)  # O(scores) * O(ranks log ranks)
    return positions


if __name__ == '__main__':
    case = '08'
    ranks_input, scores_input = io.read_case_from_file(f'input{case}.txt')

    profiler = cProfile.Profile()
    profiler.enable()
    positions = solve_problem(ranks_input, scores_input)
    profiler.disable()

    fixed_positions = list(map(lambda x: x + 1, positions))
    expected_results = io.read_results_from_file(f'output{case}.txt')
    assert expected_results == fixed_positions

    # Print the stats report
    stats = pstats.Stats(profiler)
    stats.strip_dirs()
    stats.sort_stats('cumtime')
    stats.print_stats()
