import os

i = 1
tests = []
for root, dirs, files in os.walk('./aterm-res/lmr/'):
    for name in files:
        goal = root + name if root[-1] == '/' else root + '/' + name
        print(f"testP{i} :: IO ()\ntestP{i} = runParseTest \"{goal}\"\n")
        tests.append(f"\"{goal}\" ~: testP{i}")
        i += 1

tests[0] = '[ ' + tests[0]
tests[-1] = tests[-1] + ' ]'
print('\n, '.join(tests))