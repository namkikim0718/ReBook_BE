package com.be.rebook.chat.dto;

import com.be.rebook.chat.entity.ChatMessage;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatMessageDTO { // TODO : ChatMessage 추상화
    private String senderUsername;
    private MessageType type;
    private String message;
    private Long roomId;

    // READ 전용
    private Long chatMessageId;
    private boolean isRead;

    public enum MessageType {
        JOIN, TALK, ENTER, READ, ERROR
    }

    public ChatMessageDTO(ChatMessage message) {
        this.senderUsername = message.getSenderUsername();
        this.type = ChatMessageDTO.MessageType.TALK;
        this.message = message.getMessage();
        this.roomId = message.getChatRoom().getId();
        this.isRead = message.getIsRead();
    }

    @Override
    public String toString() {
        return "ChatMessageDTO{" +
                "senderUsername=" + senderUsername +
                ", type=" + type +
                ", message='" + message + '\'' +
                ", roomId=" + roomId +
                '}';
    }
}
