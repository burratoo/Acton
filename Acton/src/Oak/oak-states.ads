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
      Setup_Cycles,                 -- 13
      New_Cycle,                    -- 14
      Release_Task,                 -- 15
      Update_Task_Property,         -- 16
      Terminated,                   -- 19
      Entering_PO,                  -- 20
      Enter_PO_Refused,             -- 21
      Exiting_PO,                   -- 22
      Exit_PO_Error,                -- 23
      Attach_Interrupt_Handler,     -- 24
      Handling_Interrupt,           -- 25
      Interrupt_Done,               -- 26
      No_State,                     -- 27
      Agent_State_Change,           -- 28
      Selecting_Next_Agent,         -- 29
      Adding_Agent,
      Removing_Agent,
      Scheduler_Agent_Done,
      Not_Initialised,
      Continue_Sleep,
      No_Message,
      Invalid_Message);

   subtype Waiting is Agent_State range
     Waiting_For_Event .. Waiting_For_Protected_Object;

   subtype Sleep is Agent_State range
     Sleeping .. Sleeping_And_Waiting;

   subtype Interrupt_States is Agent_State range
     Handling_Interrupt .. Interrupt_Done;

end Oak.States;
