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
      Waiting_For_Event,            -- 9
      Waiting_For_Protected_Object, -- 10
      Inactive,                     -- 11
      Setup_Cycles,                 -- 12
      New_Cycle,                    -- 13
      Release_Task,                 -- 14
      Update_Task_Property,         -- 15
      Terminated,                   -- 16
      Entering_PO,                  -- 17
      Enter_PO_Refused,             -- 18
      Exiting_PO,                   -- 19
      Exit_PO_Error,                -- 20
      Attach_Interrupt_Handler,     -- 21
      Handling_Interrupt,           -- 22
      Interrupt_Done,               -- 23
      No_State,                     -- 24
      Agent_State_Change,           -- 25
      Selecting_Next_Agent,         -- 26
      Adding_Agent,                 -- 27
      Adding_Agents,                -- 28
      Removing_Agent,               -- 29
      Scheduler_Agent_Done,         -- 30
      Initialising_Agents,          -- 31
      Not_Initialised,              -- 32
      Wake_Agent,                   -- 33
      No_Message,                   -- 34
      Invalid_Message);             -- 35

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

end Oak.States;
