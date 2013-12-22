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
                       Handling_Interrupt,           -- 24
                       Interrupt_Done,               -- 25
                       No_State,                     -- 26
                       Agent_State_Change,           -- 27
                       Selecting_Next_Agent,         -- 28
                       Adding_Agent,
                       Removing_Agent,
                       Scheduler_Agent_Done,
                       Continue_Sleep);

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Sleep is Agent_State range
     Sleeping .. Sleeping_And_Waiting;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

end Oak.States;
