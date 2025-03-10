import os

def printTests(tests):
    tests[0] = '[ ' + tests[0]
    tests[-1] = tests[-1] + ' ]'
    print('\n, '.join(tests))

i = 1
tests = []
for root, dirs, files in os.walk('./aterm-res/lmr/'):
    for name in files:
        goal = root + name if root[-1] == '/' else root + '/' + name
        print(f"testP{i} :: IO ()\ntestP{i} = runParseTest \"{goal}\"\n")
        tests.append(f"\"{goal}\" ~: testP{i}")
        i += 1
printTests(tests)
print()

i = 1
tests = []
for root, dirs, files in os.walk('./aterm-res/lmr/'):
    for name in files:
        goal = root + name if root[-1] == '/' else root + '/' + name
        runner = 'runTCTest' if '.no.' not in name else 'runTCFail'
        print(f"testE2E{i} :: IO ()\ntestE2E{i} = runE2ETest \"{goal}\" {runner}\n")
        tests.append(f"\"{goal}\" ~: testE2E{i}")
        i += 1
printTests(tests)
print()

i = 1
tests = []
for root, dirs, files in os.walk('./aterm-res/lmr/definitions/'):
    for name in files:
        goal = root + name
        positive = '.no.' not in name
        print(f"testB{i} :: IO ()\ntestB{i} = runBasisTest \"{goal}\" {positive} \n")
        tests.append(f"\"{goal}\" ~: testB{i}")
        i += 1
printTests(tests)
print()

i = 1
tests = []
for root, dirs, files in os.walk('./aterm-res/lmr/modules/'):
    for name in files:
        goal = root + name
        print(f"testM{i} :: IO ()\ntestM{i} = runModuleTest \"{goal}\" True \n")
        tests.append(f"\"{goal}\" ~: testM{i}")
        i += 1
printTests(tests)
print()