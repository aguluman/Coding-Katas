// Declare namespace
namespace ToyCar
{
    // Declare class
    public static class Program
    {

        // Entry point of the program
        private static void Main()
        {
            // Initialize variables
            int number;
            string buttons;

            // Read the first line from console as integer. Catch errors.
            try
            {
                number = int.Parse(Console.ReadLine() ?? throw new InvalidOperationException());
            }
            catch
            {
                Console.WriteLine("Invalid input. Please enter an integer.");
                return;
            }

            // Read the second line from console as string.
            try
            {
                buttons = Console.ReadLine() ?? throw new InvalidOperationException();
            }
            catch
            {
                Console.WriteLine("Invalid input. Please enter a sequence of '+' and '-' characters.");
                return;
            }

            // Initialize speed and distance
            int speed = 0, distance = 0;

            // Loop through characters in the string up to 'number' characters(length of the int numbers)
            foreach (var btn in buttons.Take(number))
            {
                // If the character is '+', increase the speed. Otherwise decrease it (not below zero).
                speed = btn == '+' ? speed + 1 : Math.Max(0, speed - 1);

                // Add current speed to total distance
                distance += speed;
            }

            // Output total distance
            Console.WriteLine(distance);
        }
    }
}