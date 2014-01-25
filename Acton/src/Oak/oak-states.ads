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
      Sleeping_And_Waiting,         -- 8
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
      Removing_Agent,               -- 28
      Scheduler_Agent_Done,         -- 29
      Not_Initialised,              -- 30
      Continue_Sleep,               -- 31
      No_Message,                   -- 32
      Invalid_Message);             -- 33

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Sleep is Agent_State range
     Sleeping .. Sleeping_And_Waiting;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

end Oak.States;
