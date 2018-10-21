from statemachine import StateMachine
import sys
import os
from shutil import copyfile

if len(sys.argv) < 2:
    print("USAGE: determinizer <sm1> [<sm2>] [<sm3>] ...")

for sm_filename in sys.argv[1:]:
    assert os.path.isfile(sm_filename)

    name, ext = os.path.splitext(sm_filename)
    sm_determinized_filename = f"{name}_determinized{ext}"

    sm = StateMachine.from_file(sm_filename)
    sm_determinized = sm.determinize()
    sm_determinized.save_to_file(sm_determinized_filename)
    
    sm.render(with_stock_state=False).save(f"{name}.png")
    sm.delete_epsilon_transitions().render(with_stock_state=False).save(f"{name}_no_eps.png")
    sm_determinized.render(with_stock_state=False).save(f"{name}_determinized.png")
    sm_determinized.minimize().render(with_stock_state=False).save(f"{name}_minimized.png")