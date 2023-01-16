from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		variables = set()
		if isinstance(t, Variable):
			variables.add(t)
			return variables
		elif isinstance(t, Function):
			for term in t.terms:
				variables.update(self.variables_of_term(term))
		return variables

	def variables_of_clause (self, c : Rule) -> set :
		variables = set()
		if isinstance(c.head, Function):
			for term in c.head.terms:
				variables.update(self.variables_of_term(term))
		if c.body == []:
			return variables
		if isinstance(c.body, RuleBody):
			for term in c.body.terms:
				variables.update(self.variables_of_term(term))
		return variables



	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t, Variable):
			if t in s:
				return s[t]
			else:
				return t
		elif isinstance(t, Function):
			new_terms = [self.substitute_in_term(s, term) for term in t.terms]
			return Function(t.relation, new_terms)
		return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		new_head = self.substitute_in_term(s, c.head)
		if c.body is []:
			return Rule(new_head, [])
		new_body = [self.substitute_in_term(s, term) for term in c.body.terms]
		return Rule(new_head, RuleBody(new_body))


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unifyHelper (self, t1 : Term, t2 : Term, s : dict) -> dict:
		t1 = self.substitute_in_term(s, t1)
		t2 = self.substitute_in_term(s, t2)

		if t1 == t2:
			return s
		elif isinstance(t1, Variable):
			if not self.occurs_check(t1, t2):
				s[t1] = t2
				for key in s:
					s[key] = self.substitute_in_term(s, s[key])
				return s
		elif isinstance(t2, Variable):
			if not self.occurs_check(t2, t1):
				s[t2] = t1
				for key in s:
					s[key] = self.substitute_in_term(s, s[key])
				return s
		elif isinstance(t1, Function) and isinstance(t2, Function):
			if t1.relation != t2.relation:
				raise Not_unifiable()
			elif len(t1.terms) != len(t2.terms):
				raise Not_unifiable()
			else:
				return (reduce(lambda s, xy: self.unifyHelper(xy[0], xy[1], s), zip(t1.terms, t2.terms), s))
		else:
			raise Not_unifiable()

	def unify (self, t1: Term, t2: Term) -> dict:
		return self.unifyHelper(t1, t2, {})

	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		goal = pgoal.copy()
		resolvent = goal.copy()
		
		while len(resolvent) > 0:
			random_index_goal = random.randint(0, len(resolvent) - 1)
			random_goal = resolvent[random_index_goal]
			unify_clause = []
			
			for clause in program:
				clause = self.freshen(clause)

				try:
					s = self.unify(clause.head, random_goal)
				except Not_unifiable:
					s = None

				if s is not None:
					unify_clause.append(clause)
			
			if len(unify_clause) == 0:
				break

			random_index_program = random.randint(0, len(unify_clause) - 1)
			random_clause = unify_clause[random_index_program]
			s = self.unify(random_clause.head, random_goal)

			resolvent.remove(random_goal)
			if clause.body != []:
				for function in random_clause.body.terms:
					resolvent.append(function)
			
			for index, term in enumerate(resolvent):
				term = self.substitute_in_term(s, term)
				resolvent[index] = term
			
			for index, term in enumerate(goal):
				term = self.substitute_in_term(s, term)
				goal[index] = term
	
		if len(resolvent) == 0:
			return goal
		else:
			return self.nondet_query(program, goal)


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def dfs(self, program: List[Rule], resolvent: List[Term], pgoal : List[Term], solutions:List[Term]) -> List[List[Term]]:
		if len(resolvent) == 0:
			solutions.append(pgoal)
			return solutions
		
		while len(resolvent) > 0:
			chosen_goal = resolvent[0]
			resolvent.remove(chosen_goal)

			for clause in program:
				clause = self.freshen(clause)

				try :
					s = self.unify(clause.head, chosen_goal)
				except Not_unifiable:
					s = None
				
				if s is not None:
					new_resolvent = resolvent.copy()
					new_goal = pgoal.copy()
					
					if clause.body != []:
						for function in clause.body.terms:
							new_resolvent.append(function)

					for index, term in enumerate(new_resolvent):
						term = self.substitute_in_term(s, term)
						new_resolvent[index] = term

					for index, term in enumerate(new_goal):
						term = self.substitute_in_term(s, term)
						new_goal[index] = term

					self.dfs(program, new_resolvent, new_goal, solutions)

			return solutions


	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		return self.dfs(program, pgoal.copy(), pgoal.copy(), [])
