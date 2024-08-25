package com.be.rebook.chat.dto;

import com.be.rebook.chat.entity.ChatRoom;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatRoomDto {

    private Long roomId;

    private String buyerUsername;

    private String sellerUsername;

    private Long productId;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Integer unreadCount;

    public ChatRoomDto(ChatRoom chatRoom) {
        this.roomId = chatRoom.getId();
        this.buyerUsername = chatRoom.getBuyerUsername();
        this.sellerUsername = chatRoom.getSellerUsername();
        this.productId = chatRoom.getProductId();
    }

    public String toString() {
        return "ChatRoomDto{" +
                "roomId=" + roomId +
                ", buyerUsername=" + buyerUsername +
                ", sellerUsername=" + sellerUsername +
                ", productId=" + productId +
                '}';
    }
}
