import numpy as np
from IPython.display import display, Math, Latex, Markdown
import pydot

class GlobalInfos(object):
    def __init__(self):
        self.graph = pydot.Dot("BB", graph_type="digraph")
        self._global_max_lb = np.NINF
        self.found_solution = False
        self._max_lbs = []
        self._max_lbs_r = []
        self.indexer = -1
    
    @property
    def global_max_lb(self):
        return self._global_max_lb

    def get_number(self):
        self.indexer += 1
        return self.indexer

    @global_max_lb.setter
    def global_max_lb(self, lb):
        self._global_max_lb = lb
        self._max_lbs.append(lb)

    def global_max_lb_r(self, r):
        self._global_max_lb_r = r
        self._max_lbs_r.append(r)

global_infos = GlobalInfos()

class BranchAndBoundKnapSack(object):
    def __init__(self, df, max_weight):
        df['RelativeValue'] = df['Value']/df['Weight']
        global_infos.orig_data = df
        global_infos.max_weight = max_weight
        global_infos.sorted_data = df.sort_values(by='RelativeValue', ascending=False).reset_index(drop=True)
        self._print()

    def _print(self):
        display(global_infos.orig_data.T)
        display(global_infos.sorted_data.T)
        display(global_infos.sorted_data)
        
    def run(self):
        node = BranchAndBoundNode([], [], parent=None, number=global_infos.get_number())
        node.expand()
        return global_infos



class BranchAndBoundNode(object):
    def __init__(self, index_no, index_yes, number, parent=None):
        if number > 30:
            raise Error()
        self.infeasible = False
        self.index_yes = index_yes.copy()
        self.index_no = index_no.copy()
        self.number = number
        self.parent = parent
        self._lb = None
        self._ub = None
        self._indices_in_node = index_yes
        self.partial_index = None
        self.partial_index_multiplier = None
        self.global_update = False
        self.children = []
        self.dominance = False

    def _get_ub_array(self):
        array = self._get_lb_array()
        if self.partial_index is not None:
            array[self.partial_index] = self.partial_index_multiplier
        return array

    def _get_lb_array(self):
        array = np.zeros(global_infos.sorted_data.shape[0])
        for i in self._indices_in_node:
            array[i] = 1
        return array


    def expand(self):

        display('--------')
        self._fill_by_value()
        display(Latex(f'Node $ r={self.number} $'))
        display(Latex(f"fixed_no: ({','.join([str(i+1) for i in sorted(self.index_no)])})"))
        display(Latex(f"fixed_yes: ({','.join([str(i+1) for i in sorted(self.index_yes)])})"))
        content = f"Node r = {self.number} &#13;&#10;" \
                  f"fixed_no: ({','.join([str(i+1) for i in sorted(self.index_no)])}) &#13;&#10;" \
                  f"fixed_yes: ({','.join([str(i+1) for i in sorted(self.index_yes)])}) &#13;&#10;"
        if not self.infeasible and not self.dominance:
            display(Latex(f"ub: {self._get_ub_array()} = {self.ub}"))
            display(Latex(f"lb: {self._get_lb_array()} = {self.lb}"))
            content += f"ub: {self._get_ub_array()} = {self.ub} &#13;&#10; " \
                       f"lb: {self._get_lb_array()} = {self.lb} &#13;&#10; "
        if self.infeasible:
            content += '** infeasible ** &#13;&#10;'
        if self.dominance:
            content += '**domincance** &#13;&#10;'
        if self.global_update:
            content += '**global update** &#13;&#10;'

        self.node = pydot.Node(self.number, label=content, shape="box")
        global_infos.graph.add_node(self.node)
        if self.parent is not None:
            global_infos.graph.add_edge(pydot.Edge(self.parent.number, self.number))

            
        if not self.infeasible and not self.dominance:
            if self.global_update:
                display(Latex("global update"))
            if self.partial_index is not None and not global_infos.found_solution and not self.infeasible and not self.dominance:
                node_left = BranchAndBoundNode(index_no=self.index_no.copy() + [self.partial_index], index_yes=self.index_yes.copy(), parent=self, number=global_infos.get_number())
                node_right = BranchAndBoundNode(index_no=self.index_no, index_yes=self.index_yes.copy() + [self.partial_index], parent=self, number=global_infos.get_number())
                children = [node_right, node_left]

                for child in children:
                    child.expand()
        elif self.infeasible:
            display('Infeasible')
        elif self.dominance:
            display('Dominance')
        else:
            display('No solution found?????')

            
    def _fill_by_value(self):
        weight, value, remaining_indices = self._get_value_by_index(self.index_yes, self.index_no)

        for i in remaining_indices:
            accumulated_weight = weight + global_infos.sorted_data.loc[i, 'Weight']
            if accumulated_weight < global_infos.max_weight:
                weight += global_infos.sorted_data.loc[i, 'Weight']
                self._indices_in_node.append(i)
                value += global_infos.sorted_data.loc[i, 'Value']
            elif int(accumulated_weight) == int(global_infos.max_weight):
                display(f"perfect match, {accumulated_weight}")
                self._indices_in_node.append(i)
                value += global_infos.sorted_data.loc[i, 'Value']
                self._lb = value
                self._ub = value
                if self._ub <= global_infos.global_max_lb:
                    self.dominance = True
                    return
            else:
                self._lb = value

                weight_space = abs((global_infos.sorted_data.loc[i, 'Weight'] - (accumulated_weight - global_infos.max_weight)) / global_infos.sorted_data.loc[i, 'Weight'])
                partial_value = weight_space * global_infos.sorted_data.loc[i, 'Value']
                self._ub = value + partial_value
                self.partial_index = i
                self.partial_index_multiplier = weight_space
                if self._ub < global_infos.global_max_lb:
                    self.dominance = True
                    return
                elif len(remaining_indices) <= 1:
                    self.infeasible = True
                    return
                else:
                    display(self._ub)
                    display(global_infos.global_max_lb)
                    if self._ub == global_infos.global_max_lb:
                        display('opimality')

                    if self.lb > global_infos._global_max_lb:
                        self.global_update = True
                        display(f'global update {self.lb} > {global_infos.global_max_lb}')
                        global_infos.global_max_lb = self.lb
                        global_infos.global_max_lb_r = self.number
                return
                
        self._ub = value
        self._lb = value
        if self.lb > global_infos._global_max_lb:
            self.global_update = True
            display(f'global update {self.lb} > {global_infos.global_max_lb}')
            global_infos.global_max_lb = self.lb
            global_infos.global_max_lb_r = self.number

    def _get_value_by_index(self, index_yes, index_no):
        weight = 0
        value = 0
        remaining_indices = []
        for (i, row) in global_infos.sorted_data.iterrows():
            if i in index_yes:
                weight += row['Weight']
                value += row['Value']
            elif i in index_no:
                pass
            else:
                remaining_indices.append(i)
        return weight, value, remaining_indices


    @property
    def lb(self):
        return self._lb
    
    @property
    def ub(self):
        return self._ub