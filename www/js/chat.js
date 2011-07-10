/**
* chat.js
* @author takuya-watabe
*/

// onload
$(function() {
    $('#login').submit(function() {
        var login_name = $('#name').val();
        if (login_name.length == 0) {
            alert('please input your name');
            return false;
        }

        $.post('chat_handler.yaws', 
            {
                name: login_name
            },
            success = function(data) {
            alert(data);
                // hide container
                //$("#login_container").hide();
            }
        );

        return false;
    });
    $('#get_message').submit(function() {
        $.get('chat_handler.yaws', 
            {
                Message: chat_message.getMessageId()
            },
            success = function(data) {
                var messages = JSON.parse(data);
                chat_message.showMessage(messages);
            }
        );

        return false;
    });
});

// chat message
var Chat = {};
Chat.Message = function() {
    var messageId = 0;
    var convertName = function(name) {
        var convertedName = name;
        switch (name) {
            case "system":
                convertedName = "システムメッセージ";
                break;
        }

        return convertedName;
    };
    var escape = function(val) {
        return $("<div/>").text(val).html();
    }
    return {
        getMessageId : function() {
            return messageId;
        },
        showMessage : function(messageList) {
            for(var i = 0; i < messageList.length; i++) {
                $("#chat_message").prepend(
                    "<dt id=\"message" + messageList[i].log_id + "\">" +
                        escape(messageList[i].name) +
                    "</dt>" +
                    "<dd>" + 
                        "<div>" +
                        escape(messageList[i].message) +
                        "</div>" +
                    "</dd>");
                if(messageId < messageList[i].log_id) {
                    messageId = messageList[i].log_id;
                }
            }
        }
    }
};

var chat_message = Chat.Message();
