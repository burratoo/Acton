package Oak.States with Pure is
   type Agent_State is
     (Bad_State,                    -- 0
      Activation_Pending,           -- 1
      Activation_Failed,            -- 2
      Activation_Successful,        -- 3
      Activation_Complete,          -- 4
      Running,                      -- 5
      Runnable,                     -- 6
      Sleeping,                     -- 7
      Waiting_For_Event,            -- 8
      Waiting_For_Protected_Object, -- 9
      Inactive,                     -- 10
      Setup_Cycles,                 -- 11
      New_Cycle,                    -- 12
      Release_Task,                 -- 13
      Update_Task_Property,         -- 14
      Terminated,                   -- 15
      Entering_PO,                  -- 16
      Enter_PO_Refused,             -- 17
      Exiting_PO,                   -- 18
      Exit_PO_Error,                -- 19
      Attach_Interrupt_Handler,     -- 20
      Handling_Interrupt,           -- 21
      Interrupt_Done,               -- 22
      No_State,                     -- 23
      Agent_State_Change,           -- 24
      Selecting_Next_Agent,         -- 25
      Adding_Agent,                 -- 26
      Adding_Agents,                -- 27
      Removing_Agent,               -- 28
      Scheduler_Agent_Done,         -- 29
      Allowance_Exhausted,          -- 30
      No_Agent_To_Run,              -- 31
      Initialising_Agents,          -- 32
      Not_Initialised,              -- 33
      Wake_Agent,                   -- 34
      No_Message,                   -- 35
      Invalid_Message);             -- 36

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

   subtype Deletable_State is Agent_State
     with Static_Predicate => Deletable_State in Allowance_Exhausted;
end Oak.States;
