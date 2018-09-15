from statemachine import StateMachine

sm = StateMachine.load_from_file("test_0.json")

sm.draw.show()