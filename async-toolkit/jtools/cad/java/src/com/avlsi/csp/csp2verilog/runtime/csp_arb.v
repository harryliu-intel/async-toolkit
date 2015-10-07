/*
When an arbiter is required in a non-deterministic selection statement,
cast2verilog will instantiate `CAST2VERILOG_ARBITER to do the actual
arbitration.  CAST2VERILOG_ARBITER is expected to be defined to a module that:
(1) has a single input port for _RESET
(2) has a parameter GUARDS that is the number of total guards in the
non-deterministic selection
(3) has an integer array called true_guards that has length equal to the total
number of guards
(4) has a function called select that returns the number of the guard choosen
as an integer and takes an integer argument that is the total number of guards
that are true.

Each guard is assigned a number sequentially starting from 0.  When entering a
non-deterministic selection statement, the guards will be evaluated.  Guards
that are true will have their corresponding number added to the true_guards
array in sequence.  Then the select function will be invoked, and the guard it
returns will be the guard that is selected to run.
*/

module csp_random_arbiter(input _RESET);
parameter GUARDS = 1;
integer true_guards[0:GUARDS - 1];
function integer select(input integer num_true_guards);
    select = true_guards[{$random} % num_true_guards];
endfunction
endmodule

module csp_fair_arbiter(input _RESET);
parameter GUARDS = 1;
integer true_guards[0:GUARDS - 1];
integer history[0:GUARDS - 1];
integer num_candidates;
integer candidates[0:GUARDS - 1];
integer min;
integer guard;
integer selected;
integer i;

initial begin
    init;
end

always @(negedge _RESET) begin
    init;
end

task init;
begin
    for (i = 0; i < GUARDS; i = i + 1) history[i] = 0;
end
endtask

function integer select(input integer num_true_guards);
begin
    num_candidates = 0;
    min = 1;
    for (i = 0; i < num_true_guards; i = i + 1) begin
        guard = true_guards[i];

        // increment the counter for each true guard; flip the sign of the
        // count to keep track of the true guards
        history[guard] = -history[guard] - 1;

        // keep track of all guards with the lowest count
        if (history[guard] <= min) begin
            if (history[guard] < min) begin
                min = history[guard];
                num_candidates = 0;
            end
            candidates[num_candidates] = guard;
            num_candidates = num_candidates + 1;
        end
    end

    // unflip the sign of count for true guards; reset the count of the false
    // guards to 0
    for (i = 0; i < GUARDS; i = i + 1)
        history[i] = history[i] < 0 ? -history[i] : 0;

    // select a guard with the highest count randomly
    selected = candidates[{$random} % num_candidates];

    // rest the count for the selected guard to 0
    history[selected] = 0;

    select = selected;
end
endfunction
endmodule
