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

function tell(event)
{
  event.stopPropagation();
  event.preventDefault();

  var theTarget = $(event.target);

  $.post({
    url: '/api/',
    data: JSON.stringify({
      type : "task",
      task : '456', //theTarget.closest('.task.card').data('id'),
      field: theTarget.attr('name'),
      value: theTarget.val()
    }),
    contentType: 'application/octet-stream'
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
      var info = JSON.parse(rawInfo);
      
      if (info.type === 'task')
      {
        if (info.field === 'status')
        {
          $('.task.card[data-id=' + info.task + '] .ui.task.dropdown').dropdown('set value', 'NOT_STARTED').dropdown('refresh');
        }
        else
        {
          $('.task.card[data-id=' + info.task + '] input[name=' + info.field + ']').val(info.value);
        }
      }
      
      listenUpdates(lastSID + 1);
    }
  });
}

$(document).ready(function()
{
  $('.ui.task.dropdown').dropdown({
    on: 'hover',
    action: 'select',
    onChange: function(value, text, choice) {
      if (!choice)
        return;
        
      /* Changes dropdown's *icon* to the selected one.
       * This is necessary since Semantic UI would otherwise copy the selected
       * status's *icon + text*, and that's undesirable.
       */
      choice.closest('.ui.task.dropdown')
            .children('i')
            .removeClass()
            .addClass('ui ' + statuses[value].icon + ' icon');
      
      $.post({
        url: '/api/',
        data: JSON.stringify({
          type : "task",
          task : '456', //choice.closest('.task.card').data('id'),
          field: "status",
          value: value
        }),
        contentType: 'application/octet-stream'
      });
    }
  });

  $('textarea').autoHeight();
  listenUpdates(0);
});
