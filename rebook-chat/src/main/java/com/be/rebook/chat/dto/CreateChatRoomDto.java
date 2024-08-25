package com.be.rebook.chat.dto;

import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class CreateChatRoomDto {

    private String buyerUsername;

    private String sellerUsername;

    @NotNull(message = "Product id is required")
    private Long productId;
}
