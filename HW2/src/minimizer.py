from statemachine import StateMachine
import sys
import os
from shutil import copyfile

if len(sys.argv) < 2:
    print("USAGE: minimizer <sm1> [<sm2>] [<sm3>] ...")

for sm_filename in sys.argv[1:]:
    assert os.path.isfile(sm_filename)

    name, ext = os.path.splitext(sm_filename)
    sm_minimized_filename = f"{name}_minimized{ext}"

    sm = StateMachine.from_file(sm_filename)
    sm_minimized = sm.minimize()
    if sm.get_states_count() == sm_minimized.get_states_count():
        copyfile(sm_filename, sm_minimized_filename)
    else:
        sm_minimized.save_to_file(sm_minimized_filename)
    
    sm.render().save(f"{name}.png")
    sm_minimized.render().save(f"{name}_minimized.png")