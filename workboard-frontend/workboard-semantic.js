jQuery.fn.extend({
  autoHeight: function () {
    function autoHeight_(element) {
      return jQuery(element)
        .css({ 'height': 'auto', 'overflow-y': 'hidden' })
        .height(element.scrollHeight);
    }
    return this.each(function() {
      autoHeight_(this).on('input', function() {
        autoHeight_(this);
      });
    });
  }
});

var statuses = {
  NOT_STARTED: { label: "Not started yet"  , icon: "wait"      },
  ONGOING    : { label: "Ongoing"          , icon: "play"      },
  PAUSED     : { label: "Paused"           , icon: "pause"     },
  FEEDBACK   : { label: "Awaiting feedback", icon: "help"      },
  SOLVED     : { label: "Solved"           , icon: "checkmark" },
  CLOSED     : { label: "Closed"           , icon: "thumbs up" }
}

var user = null;

var statusDropdownConfig = {
  on: 'hover',
  action: 'select',
  onChange: function(value, text, choice) {
    if (!choice)
      return;

    var statusClass = 'ui ' + statuses[value].icon + ' icon';

    var theDropdown = choice.closest('.ui.task.dropdown');
    theDropdown.children('i').removeClass().addClass(statusClass);

    sendMessage(theDropdown, {
        type  : 'task',
        task  : choice.closest('.task.card').data('id'),
        field : 'status',
        value : value,
        sender: user
      });
  }
};

function sendMessage(origin, message)
{
  if (origin.data('remote') !== 'remote')
  {
    $.post({
      url: '/api/',
      data: JSON.stringify(message),
      contentType: 'application/octet-stream'
    });
  }
}

function tell(event)
{
  event.stopPropagation();
  event.preventDefault();

  var theTarget = $(event.target);

  sendMessage(theTarget, {
    type  : "task",
    task  : theTarget.closest('.task.card').data('id'),
    field : theTarget.attr('name'),
    value : theTarget.val(),
    sender: user
  });
}

function updateTime(event)
{
  event.stopPropagation();
  event.preventDefault();

  var timeDescription = $(event.target).closest ('.time.description');
  var taskCard        = timeDescription.closest ('.task.card'       );
  var spentInput      = timeDescription.children('input[name="spentTime"]'    );
  var estimatedInput  = timeDescription.children('input[name="estimatedTime"]');

  var spent     = parseFloat(    spentInput.val());
  var estimated = parseFloat(estimatedInput.val());

  timeDescription.removeClass('punctual overflow');
  taskCard       .removeClass('green    red'     );

  if (!isNaN(spent) && !isNaN(estimated))
  {
    timeDescription.addClass((spent <= estimated) ? 'punctual' : 'overflow');
    taskCard       .addClass((spent <= estimated) ? 'green'    : 'red'     );
  }
}

$(window).keydown(function(event)
{
  if (event.altKey)
  {
    // Alt + N: New activity
    if (event.which == 78)
    {
      event.stopPropagation();
      event.preventDefault();

      createTask();
    }

    // Alt + [0-9]: toggle the corresponding activity.
    else if (49 <= event.which && event.which <= 57)
    {
      event.stopPropagation();
      event.preventDefault();
    }

    // Alt + [0-9]: toggle the corresponding activity.
    else if (97 <= event.which && event.which <= 105)
    {
      event.stopPropagation();
      event.preventDefault();
    }
  }
});

function listenUpdates(lastSID)
{
  $.get({
    url: '/api/' + lastSID,
    success: function(rawInfo) {
      listenUpdates(lastSID + 1);

      var info = JSON.parse(rawInfo);

      if (info.sender === user)
        return;

      if (info.type === 'task')
      {
        if (info.field === 'status')
        {
          $('.task.card[data-id=' + info.task + '] .ui.task.dropdown')
            .data    ('remote'      , 'remote'  )
            .dropdown('set selected', info.value)
            .data    ('remote'      , 'user'    );
        }
        else
        {
          $('.task.card[data-id=' + info.task + '] *[name=' + info.field + ']')
            .data   ('remote', 'remote')
            .val    (info.value        )
            .trigger('input'           )
            .data   ('remote', 'user'  );
        }
      }
    }
  });
}

function createTask()
{
  var theClone = $('.templates .task.card').clone();

  theClone.find('.ui.task.dropdown')
          .dropdown(statusDropdownConfig)
          .data    ('remote'      , 'remote'     )
          .dropdown('set selected', 'NOT_STARTED')
          .data    ('remote'      , 'user'       );

  theClone.find('textarea').autoHeight();

  theClone.attr('data-id', Math.random().toString())
          .appendTo('.ui.cards')
          .transition('scale');
}

function removeTask(event)
{
  $(event.target).closest('.task.card').remove();
}

$(document).ready(function()
{
  listenUpdates(0);
});
