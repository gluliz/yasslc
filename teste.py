grammar="""S E $
E E + T
E T
T T * F
T F
F ( E )
F id"""

rules = grammar.split("\n")

rules = [rule.split(" ") for rule in rules]

def is_terminal(token):
  return not token.isupper()

states=[[(0,1,1)]]
max_state=1
curr_state = 0
curr_pos = 0
while (curr_state < len(states)):
  while (curr_pos < len(states[curr_state])):
    #string a direita do ponto
    if (states[curr_state][curr_pos][2]>0):
      dot = rules[states[curr_state][curr_pos][0]][states[curr_state][curr_pos][1]]
    #verifica se as regras com essa string a esquerda ja foram adicionadas
    is_in_state = True
    for i in range(len(rules)):
      if rules[i][0] == dot:
        present = False
        for j in states[curr_state]:
          if j[0] == i:
            present = True
        if (not present):
          is_in_state = False
          break
    #for i in states[curr_state]:
    #  if rules[i[0]][0] == dot:
    #    is_in_state = True
    #    break
    if (is_terminal(dot)):
      is_in_state = True
    if (not is_in_state):
      for i in range(len(rules)):
        if (rules[i][0] == dot):
          found = False
          for state in states[curr_state]:
            if (state[2] > 0 and rules[i][1] == rules[state[0]][state[1]]):
              states[curr_state].append((i, 1, state[2]))
              found = True
              break
          if (not found):
            new_state=[]
            for state in states:
              for j in state:
                if (j[0] == i and j[1] == 1):
                  new_state=(i, 1, j[2])
                  found=True
            if (found):
              states[curr_state].append(new_state)
          if (not found):
            max_state+=1
            states[curr_state].append((i, 1, max_state))
    curr_pos+=1
  curr_state+=1
  new_state=[]
  for state in states:
    for i in state:
      if (i[2] == curr_state):
        if (i[1]+1 == len(rules[i[0]])):
          if (rules[i[0]][i[1]] == "$"):
            new_state.append((i[0], i[1]+1, 0))
          else:
            new_state.append((i[0], i[1]+1, -i[0]))    
        else:
          found=False
          for state in states:
            for j in state:
              if (j[0]==i[0] and j[1] == i[1]+1 and not found):
                new_state.append((i[0], i[1]+1, j[2]))
                found=True
          if (not found):
            max_state+=1
            new_state.append((i[0], i[1]+1, max_state))
  if (len(new_state)>0):
    states.append(new_state)
  curr_pos=0
with open("vilson", "w") as f:
  f.write(str(states))