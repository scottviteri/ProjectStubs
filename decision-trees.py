import math
import functools
import random

def classify(dtree, preds, key, rec_depth=0):
    if isinstance(dtree, bool): return dtree
    pred = preds[rec_depth]
    value = pred(key)
    return classify(dtree[value], preds, key, rec_depth = rec_depth + 1)

dtree = {"a":True, "b":{1:False, 2:True}}
preds = [lambda x: x[0], lambda x:x[1]]

assert(classify(dtree, preds, ["a",1])) # True
assert(not(classify(dtree, preds, ["b",1]))) # False
assert(classify(dtree, preds, ["b",2])) # True

def entropy(probs):
    return sum(map(lambda p: -p*math.log2(p), probs))
def binary_entropy(p): return entropy([p, 1.0-p])

# I want to sort many examples based on results on classification

# create logical formulas
boolean_formula = [[],[0,2,4]] # 1 xor (x_0 & x_2 & x_4)
def nary_and(lst): return functools.reduce(lambda x, y: x & y, lst, True)
def nary_xor(lst): return functools.reduce(lambda x, y: x ^ y, lst, False)

def eval_formula(bformula, assignments): #method based on bool polynomials
    return nary_xor([nary_and(map(lambda i: assignments[i], term))
                     for term in bformula])

assert(not(eval_formula([], []))) #not(False)
assert(eval_formula([[]], [])) #True
# formula x_0 <-> variable x_0
assert(eval_formula([[0]], [True]) and not(eval_formula([[0]], [False])))
assert(eval_formula([[1,2]], [False, True, True])) #x_1 & x_2
assert(not(eval_formula([[],[1,2]], [False, True, True]))) # 1 ^ x_1 & x_2

def flatten(lsts): return functools.reduce(lambda l1, l2: l1 + l2, lsts, [])
def gen_rand_vars(num_vars): return [bool(random.randint(0,1))
                                     for _ in range(num_vars)]
def gen_unique_vars(num_vars, num_examples):
    assignments = []
    while len(assignments) < num_examples:
        next_val = gen_rand_vars(num_vars)
        if next_val not in assignments:
            assignments.append(next_val)
    return assignments

def generate_examples(bformula, num_examples):
    flat_bformula = flatten(bformula)
    num_vars = max(flat_bformula)+1 if flat_bformula else 0
    return gen_unique_vars(num_vars, num_examples)

# generate_examples([[0,2]], 2) # 2 different boolean lists of length 3
# generate_examples(boolean_formula, 5)
examples = gen_unique_vars(5, 10)
is_true = lambda x: x==True
labeled_examples = list(map(lambda e:(eval_formula(boolean_formula, e), e), examples))
