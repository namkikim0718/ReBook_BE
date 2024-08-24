package com.be.rebook.chat.dto;

import com.be.rebook.chat.entity.ChatMessage;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatMessageDTO {
    private Long senderId;
    private MessageType type;
    private String message;
    private Long roomId;
    private boolean isRead;

    public enum MessageType {
        JOIN, TALK, ENTER, ERROR
    }

    public ChatMessageDTO(ChatMessage message) {
        this.senderId = message.getSenderId();
        this.type = ChatMessageDTO.MessageType.TALK;
        this.message = message.getMessage();
        this.roomId = message.getChatRoom().getId();
        this.isRead = message.getIsRead();
    }

    @Override
    public String toString() {
        return "ChatMessageDTO{" +
                "senderId=" + senderId +
                ", type=" + type +
                ", message='" + message + '\'' +
                ", roomId=" + roomId +
                '}';
    }
}
