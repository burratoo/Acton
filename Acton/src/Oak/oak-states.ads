package Oak.States with Pure is
   type Agent_State is (
                       Bad_State,                    -- 0
                       Activation_Pending,           -- 1
                       Activation_Failed,            -- 2
                       Activation_Successful,        -- 3
                       Activation_Complete,          -- 4
                       Running,                      -- 5
                       Runnable,                     -- 6
                       Sleeping,                     -- 7
                       Sleeping_And_Waiting,         -- 8
                       Waiting_For_Event,            -- 9
                       Waiting_For_Protected_Object, -- 10
                       Inactive,                     -- 11
                       Shared_State,                 -- 12
                       Setup_Cycles,                 -- 13
                       New_Cycle,                    -- 14
                       Release_Task,                 -- 15
                       Change_Cycle_Period,          -- 16
                       Change_Relative_Deadline,     -- 17
                       Terminated,                   -- 18
                       Entering_PO,                  -- 19
                       Enter_PO_Refused,             -- 20
                       Exiting_PO,                   -- 21
                       Exit_PO_Error,                -- 22
                       Attach_Interrupt_Handlers,    -- 23
                       Entering_Atomic_Action,       -- 24
                       Enter_Atomic_Action_Refused,  -- 25
                       Exiting_Atomic_Action,        -- 26
                       Exit_Atomic_Action_Error,     -- 27
                       Entering_Exit_Barrier,        -- 28
                       Atomic_Action_Error,          -- 29
                       Handling_Interrupt,           -- 30
                       Interrupt_Done,               -- 31
                       No_State,                     -- 32
                       Agent_State_Change,           -- 33
                       Selecting_Next_Agent,         -- 34
                       Adding_Agent,
                       Removing_Agent,
                       Scheduler_Agent_Done,
                       Continue_Sleep,
                       Adding_Agent_To_Scheduler);

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Sleep is Agent_State range
     Sleeping .. Sleeping_And_Waiting;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

end Oak.States;
