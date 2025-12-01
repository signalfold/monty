ellipsis_id = id(...)
none_id = id(None)
false_id = id(False)
true_id = id(True)
number_id = id(0)
# TODO: use a set once they're supported
unique = {}
unique[ellipsis_id] = True
unique[none_id] = True
unique[false_id] = True
unique[true_id] = True
unique[number_id] = True
len(unique)
# Return=5
