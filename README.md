# Collaborative workboard

A web application to let people in a team manage and share daily tasks.

The workboard we use at work is based on a shared Google Sheets document.
However, some features we'd like are not implementable on Google Sheets:

  * **Integration with Redmine.** 
    We work with Redmine tickets, but its status, spent time and estimated time
    must be placed in the workboard manually. This has been proven to be highly
    error-prone.
    
  * **Structured information.**
    Depending on the "type" and status of an entry in the workboard, it should
    follow a pre-defined format. However, this is error prone.
    
  * **Keyboard-friendly use.**
    For power users, it'd be more efficient to do common tasks without lifting
    the hands from the keyboard.
  
The idea is to implement a webapp that works like a Google Sheets document (you
get updates "in real time"), but implements the "feature wishlist" above.
