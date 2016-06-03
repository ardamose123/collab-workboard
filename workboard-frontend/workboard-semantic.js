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

function prettyHour(number)
{
  var hours, minutes, result;

  hours   = Math.floor(number);
  minutes = Math.floor(60 * (number - hours));

  if (hours === 0 && minutes === 0)
  return 'None';

  result  = (hours === 0) ? '' : (hours === 1) ? '1 hour' : hours + ' hours'  ;
  result += (hours === 0 || minutes === 0) ? '' : ', ';
  result += (minutes === 0) ? '' : minutes + ' minutes';

  return result;
}

function renderTask(task)
{
  var project = (!!task.project) ? ' <span class="grey-text text-darken-1">(' + task.project + ')</span>' : '';
  var metadata = (typeof task.metadata === 'undefined') ? '' : task.metadata;
  var timeColor;
  var timeOverflowText;

  if (task.spentTime > task.estimatedTime)
  {
    timeColor = 'overflow';
    timeOverflowText = ' <span class="' + timeColor + '">(overflow)</span>';
  }
  else
  {
    timeColor = 'punctual';
    timeOverflowText = '';
  };

  return '<div class="title">' +
  '<i class="' + statuses[task.status].icon + ' icon"></i>' +
  task.title + project +
  '<span class="right-floated ' + timeColor + '">' + task.spentTime.toFixed(2) + ' / ' + task.estimatedTime.toFixed(2) + 'h</span>' +
  '</div>' +
  '<div class="content">' +
  '<p class="as-is">' + task.description + '</p>' +
  '<table class="detail">' +
  '<tr><td>Status:        </td><td>' + statuses[task.status].label                   + '</td></tr>' +
  '<tr><td>Spent time:    </td><td>' + prettyHour(task.spentTime) + timeOverflowText + '</td></tr>' +
  '<tr><td>Estimated time:</td><td>' + prettyHour(task.estimatedTime)                + '</td></tr>' +
  metadata +
  '</table>' +
  '</div>';
}

function renderActivity(activity)
{
  return renderTask({
    title: activity.details.title,
    project: activity.details.project,
    status: activity.status,
    description: activity.details.description,
    estimatedTime: activity.estimatedTime,
    spentTime: activity.spentTime
  });
}

function renderTechTalk(talk)
{
  return renderTask({
    title: 'Tech talk',
    project: null,
    status: talk.status,
    description: '&ldquo;' + talk.details.topic + '&rdquo;<br>By ' + talk.details.speaker,
    estimatedTime: talk.estimatedTime,
    spentTime: talk.spentTime
  });
}

function renderRedmineTicket(ticket)
{
  return renderTask({
    title: '#' + ticket.details.id,
    project: ticket.project,
    status: ticket.status,
    description: ticket.title,
    estimatedTime: ticket.estimatedTime,
    spentTime: ticket.spentTime,
    metadata: '<tr><td><a href="https://tracker.alkaid.cr/issues/' + ticket.id + '" target="__blank">Check this ticket on Redmine</a></td></tr>'
  });
}

function renderUser(user)
{
  return '<a class="blue item" onclick="renderWorkboard(\'' + user.name + '\')">' + user.name + '<span class="ui label">' + user.points + '</span></a>';
}

function addNewActivity(event, activity)
{
  event.preventDefault();
  event.stopPropagation();

  activity.spentTime = parseFloat(activity.spentTime);
  activity.estimatedTime = parseFloat(activity.estimatedTime);

  $('#tasks').append(renderTask(activity));
  $(event.target).form('clear');
  $('#activityTitle').focus();
}

function showWorkboardState(state)
{
  var userList = Object.keys(state).map(function(user) { return { name: user, points: 0 }; });

  $('#user-list').html(userList.map(renderUser).join(''));
}

function listenForUpdates(updateChannel)
{
  getUpdates(updateChannel, {
    success: function(allUpdates)
    {
      console.log(allUpdates);
      setTimeout(function() { listenForUpdates(updateChannel) }, 2000);
    }
  });
}

$(window).keydown(function(event)
{
  if (event.altKey)
  {
    // Alt + N: New activity
    if (event.which == 78)
    {
      $('.ui.accordion').accordion('toggle', 0);
      $('#activityTitle').focus();
      event.stopPropagation();
      event.preventDefault();
    }

    // Alt + [0-9]: toggle the corresponding activity.
    else if (49 <= event.which && event.which <= 57)
    {
      $('.ui.accordion').accordion('toggle', event.which - 48);
      event.stopPropagation();
      event.preventDefault();
    }

    // Alt + [0-9]: toggle the corresponding activity.
    else if (97 <= event.which && event.which <= 105)
    {
      $('.ui.accordion').accordion('toggle', event.which - 96);
      event.stopPropagation();
      event.preventDefault();
    }
  }
});

$(document).ready(function()
{
  $('#user-list').html();

  $('.ui.task.dropdown').dropdown({
    on: 'hover',
    action: 'select',
    onChange: function(value, text, choice) {
      choice.closest('.ui.task.dropdown')
            .children('i')
            .removeClass()
            .addClass('ui ' + statuses[value].icon + ' icon');
    }
  });

  $('.ui.task.dropdown').dropdown('set selected', 'NOT_STARTED');
  $('textarea').autoHeight();

  $('.ui.accordion').accordion();

  $('.ui.form').form({
    fields: {
      activityTitle         : 'empty',
      activityStatus        : 'empty',
      activitySpentTime     : ['empty', 'decimal'],
      activityEstimatedTime : ['empty', 'decimal'],
      activityDescription   : 'empty'
    },
    onSuccess: addNewActivity
  });

  // createChannel({
  //     success: function(data)
  //     {
  //         var workboardInitialState = data[0];
  //         var updateChannel = data[1];
  //
  //         showWorkboardState(workboardInitialState);
  //         listenForUpdates(updateChannel);
  //     }
  // });
});
